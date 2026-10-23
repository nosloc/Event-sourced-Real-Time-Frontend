package prototype.server.demo

import java.text.Normalizer
import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import java.util.{Locale, UUID}

import cats.effect.{IO, Ref}
import cats.effect.std.Random
import cats.syntax.all.*

import prototype.api.{Address, AppointmentType, PrestationStatus, Role}

import net.datafaker.Faker

/** A prestation of a past appointment. `steps` are the status changes made
  * after its creation, in order (Nil: it stays TO_BILL).
  */
final case class PlannedPrestation(
    id: UUID,
    label: String,
    unitPrice: Float,
    steps: List[PrestationStatus]
)

/** An appointment of one therapist. `patients` are indexes into that
  * therapist's `patients`.
  */
final case class PlannedAppointment(
    id: UUID,
    apptType: AppointmentType,
    patients: List[Int],
    start: Instant,
    end: Instant,
    prestations: List[PlannedPrestation]
)

final case class PlannedPatient(
    id: UUID,
    firstName: String,
    lastName: String,
    birthDate: LocalDate
)

final case class PlannedTherapist(
    trigram: String,
    firstName: String,
    lastName: String,
    role: Role,
    patients: List[PlannedPatient],
    appointments: List[PlannedAppointment]
)

final case class PlannedCompany(
    id: UUID,
    name: String,
    address: Address,
    therapists: List[PlannedTherapist]
)

/** Everything one generation writes, ids included, drawn in memory before
  * anything is written.
  */
final case class DemoPlan(companies: List[PlannedCompany])

object DemoPlan:

  // Swiss towns with their canton: Datafaker's Swiss locales give cantons, not
  // towns, so the address is built from this list
  private val towns: List[(String, String)] = List(
    "Lausanne" -> "VD",
    "Yverdon-les-Bains" -> "VD",
    "Montreux" -> "VD",
    "Nyon" -> "VD",
    "Genève" -> "GE",
    "Carouge" -> "GE",
    "Fribourg" -> "FR",
    "Bulle" -> "FR",
    "Neuchâtel" -> "NE",
    "La Chaux-de-Fonds" -> "NE",
    "Sion" -> "VS",
    "Martigny" -> "VS",
    "Bern" -> "BE",
    "Biel/Bienne" -> "BE"
  )

  private val prestationCatalogue: List[(String, Float)] = List(
    "Consultation" -> 150f,
    "Psychotherapy session" -> 160f,
    "Assessment" -> 220f,
    "Report" -> 90f,
    "Group session" -> 60f
  )

  // Out of 100; the patients of each type come from the therapist's caseload
  private val typeWeights: List[(AppointmentType, Int)] = List(
    AppointmentType.INDIVIDUAL -> 70,
    AppointmentType.NEW_PATIENT -> 10,
    AppointmentType.COUPLE -> 10,
    AppointmentType.FAMILY -> 5,
    AppointmentType.GROUP -> 5
  )

  private def patientsPerType(apptType: AppointmentType): Int =
    apptType match
      case AppointmentType.COUPLE => 2
      case AppointmentType.FAMILY => 3
      case AppointmentType.GROUP  => 4
      case _                      => 1

  private val oldestBirthDate = LocalDate.of(1945, 1, 1)
  private val youngestBirthDate = LocalDate.of(2010, 12, 31)

  /** Draws the whole demo world. Trigrams in `takenTrigrams` (and `dev`) are
    * never used. Appointments run from `HistoryWeeks` before `today` to
    * `FutureWeeks` after it; only appointments of days before `today` get
    * prestations.
    */
  def build(
      random: Random[IO],
      today: LocalDate,
      takenTrigrams: Set[String]
  ): IO[DemoPlan] =
    for
      // The French locale gives the cleanest names; Datafaker uses its own
      // random source, so every generation differs
      faker <- IO.delay(Faker(Locale.FRENCH))
      trigrams <- Ref.of[IO, Set[String]](takenTrigrams + "dev")
      patientKeys <- Ref.of[IO, Set[(String, String, LocalDate)]](Set.empty)
      days = DayPlanner.weekdays(
        today.minusWeeks(DemoDataSizes.HistoryWeeks.toLong),
        today.plusWeeks(DemoDataSizes.FutureWeeks.toLong)
      )
      companies <- (1 to DemoDataSizes.Companies).toList.traverse(_ =>
        company(random, faker, trigrams, patientKeys, days, today)
      )
    yield DemoPlan(companies)

  private def company(
      random: Random[IO],
      faker: Faker,
      trigrams: Ref[IO, Set[String]],
      patientKeys: Ref[IO, Set[(String, String, LocalDate)]],
      days: List[LocalDate],
      today: LocalDate
  ): IO[PlannedCompany] =
    for
      id <- IO.randomUUID
      name <- IO.delay(faker.company().name())
      street <- IO.delay(faker.address().streetName())
      number <- random.betweenInt(1, 151)
      (town, canton) <- random.elementOf(towns)
      count <- random.betweenInt(
        DemoDataSizes.TherapistsPerCompanyMin,
        DemoDataSizes.TherapistsPerCompanyMax + 1
      )
      therapists <- (1 to count).toList.traverse(_ =>
        therapist(random, faker, trigrams, patientKeys, days, today)
      )
    yield PlannedCompany(
      id,
      name,
      Address(street, number.toString, town, canton),
      therapists
    )

  private def therapist(
      random: Random[IO],
      faker: Faker,
      trigrams: Ref[IO, Set[String]],
      patientKeys: Ref[IO, Set[(String, String, LocalDate)]],
      days: List[LocalDate],
      today: LocalDate
  ): IO[PlannedTherapist] =
    for
      (firstName, lastName, trigram) <- uniqueTrigram(faker, trigrams)
      role <- random.elementOf(Role.values)
      patients <- (1 to DemoDataSizes.PatientsPerTherapist).toList.traverse(_ =>
        patient(random, faker, patientKeys)
      )
      hasAppointments <- random.nextDouble.map(
        _ < DemoDataSizes.ShareOfTherapistsWithAppointments
      )
      appointments <-
        if hasAppointments then
          days.flatTraverse(appointmentsOfDay(random, patients.size, _, today))
        else IO.pure(Nil)
    yield PlannedTherapist(
      trigram,
      firstName,
      lastName,
      role,
      patients,
      appointments
    )

  // Draws names until their trigram, or a variant of it, is free, and takes it
  private def uniqueTrigram(
      faker: Faker,
      trigrams: Ref[IO, Set[String]]
  ): IO[(String, String, String)] =
    for
      firstName <- IO.delay(faker.name().firstName())
      lastName <- IO.delay(faker.name().lastName())
      base = trigramOf(firstName, lastName)
      candidates = base :: ('a' to 'z').toList.map(base.take(2) + _)
      taken <- trigrams.modify { used =>
        candidates.find(!used.contains(_)) match
          case Some(free) => (used + free, Some(free))
          case None       => (used, None)
      }
      result <- taken match
        case Some(trigram) => IO.pure((firstName, lastName, trigram))
        case None          => uniqueTrigram(faker, trigrams)
    yield result

  // First letter of the first name and first two of the last name, lower
  // case, accents removed ("Céline Dupré" gives "cdu")
  private def trigramOf(firstName: String, lastName: String): String =
    def letters(name: String): String =
      Normalizer
        .normalize(name, Normalizer.Form.NFD)
        .replaceAll("[^A-Za-z]", "")
        .toLowerCase(Locale.ROOT)
    (letters(firstName).take(1) + letters(lastName).take(2)).padTo(3, 'x')

  // A patient is refused if another has the same names and birth date: keep
  // them unique within the plan (a patient created by hand with the same
  // identity is still refused, and counted as a skipped write)
  private def patient(
      random: Random[IO],
      faker: Faker,
      patientKeys: Ref[IO, Set[(String, String, LocalDate)]]
  ): IO[PlannedPatient] =
    for
      firstName <- IO.delay(faker.name().firstName())
      lastName <- IO.delay(faker.name().lastName())
      epochDay <- random.betweenLong(
        oldestBirthDate.toEpochDay,
        youngestBirthDate.toEpochDay + 1
      )
      birthDate = LocalDate.ofEpochDay(epochDay)
      isNew <- patientKeys.modify { used =>
        val key = (firstName, lastName, birthDate)
        if used.contains(key) then (used, false) else (used + key, true)
      }
      result <-
        if isNew then
          IO.randomUUID.map(PlannedPatient(_, firstName, lastName, birthDate))
        else patient(random, faker, patientKeys)
    yield result

  private def appointmentsOfDay(
      random: Random[IO],
      caseloadSize: Int,
      day: LocalDate,
      today: LocalDate
  ): IO[List[PlannedAppointment]] =
    for
      count <- random.betweenInt(
        DemoDataSizes.AppointmentsPerDayMin,
        DemoDataSizes.AppointmentsPerDayMax + 1
      )
      slots <- DayPlanner.day(random, day, count)
      appointments <- slots.traverse { (start, end) =>
        for
          id <- IO.randomUUID
          apptType <- weightedType(random)
          patients <- random
            .shuffleList((0 until caseloadSize).toList)
            .map(_.take(patientsPerType(apptType)))
          prestations <-
            if day.isBefore(today) then
              prestationsOf(random, ChronoUnit.DAYS.between(day, today))
            else IO.pure(Nil)
        yield PlannedAppointment(
          id,
          apptType,
          patients,
          start,
          end,
          prestations
        )
      }
    yield appointments

  private def weightedType(random: Random[IO]): IO[AppointmentType] =
    // Each type owns the draws below its running total of weights
    val upperBounds =
      typeWeights.map(_._1).zip(typeWeights.scanLeft(0)(_ + _._2).tail)
    random.betweenInt(0, typeWeights.map(_._2).sum).map { draw =>
      upperBounds
        .collectFirst { case (apptType, upTo) if draw < upTo => apptType }
        .getOrElse(AppointmentType.INDIVIDUAL)
    }

  private def prestationsOf(
      random: Random[IO],
      ageInDays: Long
  ): IO[List[PlannedPrestation]] =
    for
      two <- random.nextDouble.map(_ < DemoDataSizes.ShareWithTwoPrestations)
      prestations <- (1 to (if two then 2 else 1)).toList.traverse { _ =>
        for
          id <- IO.randomUUID
          (label, unitPrice) <- random.elementOf(prestationCatalogue)
          steps <- statusSteps(random, ageInDays)
        yield PlannedPrestation(id, label, unitPrice, steps)
      }
    yield prestations

  // The final status mostly follows the age of the appointment; a few are
  // cancelled, and a few others end in any other status, whatever their age.
  // Every step is one the status rule allows. Only past appointments get
  // prestations, so this never applies after today.
  private def statusSteps(
      random: Random[IO],
      ageInDays: Long
  ): IO[List[PrestationStatus]] =
    import PrestationStatus.*
    val byAge =
      if ageInDays <= DemoDataSizes.ToBillDays then TO_BILL
      else if ageInDays <= DemoDataSizes.BilledDays then BILLED
      else PAID
    for
      offTheRule <- random.nextDouble.map(_ < DemoDataSizes.ShareOffTheRule)
      cancelled <- random.nextDouble.map(_ < DemoDataSizes.ShareCancelled)
      other <- random.elementOf(PrestationStatus.values.filterNot(_ == byAge))
      billedFirst <- random.nextBoolean
      finalStatus =
        if offTheRule then other
        else if cancelled then CANCELLED
        else byAge
    yield finalStatus match
      case TO_BILL   => Nil
      case BILLED    => List(BILLED)
      case PAID      => List(BILLED, PAID)
      case CANCELLED =>
        if billedFirst then List(BILLED, CANCELLED) else List(CANCELLED)
