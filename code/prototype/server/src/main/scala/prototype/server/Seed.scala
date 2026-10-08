package prototype.server

import java.time.DayOfWeek.*
import java.time.temporal.TemporalAdjusters
import java.time.{
  Duration,
  LocalDate,
  LocalTime,
  OffsetDateTime,
  ZoneId,
  ZonedDateTime
}
import java.util.UUID

import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.all.*
import prototype.api.*
import smithy4s.time.Timestamp

/** Fictional dummy data loaded at boot. Build it once at startup with `Seed()`,
  * which also generates tst's random appointments, then read its values.
  */
final class Seed private (generatedAppointments: List[Appointment]):
  val companies: List[Company] = Seed.companies
  val therapists: List[Therapist] = Seed.therapists
  val appointments: List[Appointment] =
    Seed.fixedAppointments ++ generatedAppointments

/** Ids are defined once and reused, so the lists always reference each other
  * consistently.
  */
object Seed:

  /** How much the number of generated appointments per day varies, as a
    * fraction of `perDay`, rounded up (e.g. 0.25 with perDay = 2 gives ±1).
    */
  val perDayVariance: Double = 0.25

  /** tst gets about 2 appointments per weekday over last, current and next week
    * (around 30 in total), relative to the boot date.
    */
  def apply(): IO[Seed] =
    IO.realTimeInstant.flatMap { now =>
      val thisMonday = now
        .atZone(zone)
        .toLocalDate
        .`with`(TemporalAdjusters.previousOrSame(MONDAY))
      generateAppointments(
        tst,
        from = thisMonday.minusWeeks(2),
        to = thisMonday.plusWeeks(2).`with`(FRIDAY),
        perDay = 3
      ).map(new Seed(_))
    }

  private def uuid(value: String): UUID = UUID.fromString(value)

  // Times are written in Swiss local time (UTC+2 until 25 Oct 2026) and stored as instants.
  private def at(dateTime: String): Timestamp =
    Timestamp.fromOffsetDateTime(OffsetDateTime.parse(dateTime))

  private def range(start: String, end: String): TimeRange =
    TimeRange(at(start), at(end))

  // Companies
  val exampleClinic: Company = Company(
    CompanyId(uuid("13ddd512-9f19-4af3-b5a9-af79bd82411e")),
    "Example Clinic",
    Address("Example Street", "1", "Exampleville", "VD")
  )
  val demoHealthCenter: Company = Company(
    CompanyId(uuid("070874c8-5296-452e-8c91-cfaf54385932")),
    "Demo Health Center",
    Address("Sample Avenue", "2", "Sampletown", "GE")
  )
  private val companies: List[Company] = List(exampleClinic, demoHealthCenter)

  // Therapists
  private def therapist(
      trigram: String,
      firstName: String,
      lastName: String,
      company: Company,
      role: Role
  ): Therapist =
    Therapist(
      firstName,
      lastName,
      TherapistId(trigram),
      company.companyId,
      role
    )

  val arb =
    therapist("arb", "Alexandre", "Robert", exampleClinic, Role.PSYCHOLOGIST)
  val mdp = therapist(
    "mdp",
    "Marie",
    "Dupont",
    demoHealthCenter,
    Role.NEUROPSYCHOLOGIST
  )
  val jma =
    therapist("jma", "Jean", "Martin", exampleClinic, Role.GENERAL_PRACTITIONER)
  val sda =
    therapist("sda", "Sophie", "Durand", demoHealthCenter, Role.PSYCHOLOGIST)
  val lbo = therapist("lbo", "Lucas", "Bonvin", exampleClinic, Role.PHYSICIAN)
  val fgr =
    therapist("fgr", "Fanny", "Grandjean", demoHealthCenter, Role.PSYCHOLOGIST)
  val nle =
    therapist("nle", "Nicolas", "Leuba", exampleClinic, Role.NEUROPSYCHOLOGIST)
  val tpe = therapist(
    "tpe",
    "Tania",
    "Perret",
    demoHealthCenter,
    Role.GENERAL_PRACTITIONER
  )
  // Test user, whose appointments are generated at boot (see apply)
  val tst = therapist("tst", "Test", "User", exampleClinic, Role.PSYCHOLOGIST)

  private val therapists: List[Therapist] =
    List(arb, mdp, jma, sda, lbo, fgr, nle, tpe, tst)

  // Fixed appointments: arb has 3, mdp has 2, jma/sda/lbo have 1 each, fgr/nle/tpe have
  // none (tst gets generated ones, see apply)
  private def appointment(
      id: String,
      therapist: Therapist,
      timeRange: TimeRange
  ): Appointment =
    Appointment(AppointmentId(uuid(id)), therapist.therapistId, timeRange)

  private val fixedAppointments: List[Appointment] = List(
    appointment(
      "9fc986ad-fbf1-40f3-9468-a44d1a5fb177",
      arb,
      range("2026-10-12T09:00:00+02:00", "2026-10-12T09:45:00+02:00")
    ),
    appointment(
      "db842a24-833c-40d8-9509-dea565e21576",
      arb,
      range("2026-10-12T14:00:00+02:00", "2026-10-12T15:00:00+02:00")
    ),
    appointment(
      "2b738424-f9ad-4c93-b8da-3d7ead6051a6",
      arb,
      range("2026-10-14T10:30:00+02:00", "2026-10-14T11:15:00+02:00")
    ),
    appointment(
      "51a05d9a-71bd-42fd-a2f9-48c51927cab0",
      mdp,
      range("2026-10-13T08:30:00+02:00", "2026-10-13T09:30:00+02:00")
    ),
    appointment(
      "279eee00-7029-498b-989a-07bbf0f6353e",
      mdp,
      range("2026-10-15T13:00:00+02:00", "2026-10-15T14:30:00+02:00")
    ),
    appointment(
      "c1db1663-b894-4e8f-a97f-da78edce9722",
      jma,
      range("2026-10-13T11:00:00+02:00", "2026-10-13T11:30:00+02:00")
    ),
    appointment(
      "8bbc36b7-a8fc-467c-85eb-cf0bbd97b85a",
      sda,
      range("2026-10-14T16:00:00+02:00", "2026-10-14T17:00:00+02:00")
    ),
    appointment(
      "ba7511ac-50ed-4a2c-89ae-84080fc5ebf0",
      lbo,
      range("2026-10-16T09:00:00+02:00", "2026-10-16T09:30:00+02:00")
    )
  )

  // Generated appointments
  private val zone: ZoneId = ZoneId.of("Europe/Zurich")
  private val dayStart: LocalTime = LocalTime.of(8, 0)
  private val dayEnd: LocalTime = LocalTime.of(18, 0)
  private val slotMinutes: Int = 15
  private val durationsMinutes: List[Int] = List(30, 45, 60, 90)

  // Every possible start time of the day: 08:00, 08:15, ..., 17:45
  private val slotStarts: List[LocalTime] =
    val slotCount =
      Duration.between(dayStart, dayEnd).toMinutes.toInt / slotMinutes
    List.tabulate(slotCount)(i => dayStart.plusMinutes(i.toLong * slotMinutes))

  /** Random, non-overlapping appointments for `therapist` on every weekday from
    * `from` to `to` (both inclusive). Each day gets between `perDay - v` and
    * `perDay + v` appointments, uniformly, with `v = ceil(perDay *
    * perDayVariance)` (fewer if the day is full). They start on a 15-minute
    * step and end by 18:00.
    */
  private def generateAppointments(
      therapist: Therapist,
      from: LocalDate,
      to: LocalDate,
      perDay: Int
  ): IO[List[Appointment]] =
    val weekdays = Iterator
      .iterate(from)(_.plusDays(1))
      .takeWhile(!_.isAfter(to))
      .filterNot(day =>
        day.getDayOfWeek == SATURDAY || day.getDayOfWeek == SUNDAY
      )
      .toList
    Random.scalaUtilRandom[IO].flatMap { random =>
      weekdays.flatTraverse(generatedDay(random, therapist, _, perDay))
    }

  private def generatedDay(
      random: Random[IO],
      therapist: Therapist,
      day: LocalDate,
      perDay: Int
  ): IO[List[Appointment]] =
    val variance = math.ceil(perDay * perDayVariance).toInt
    for
      count <- random
        .betweenInt(perDay - variance, perDay + variance + 1)
        .map(_.max(0))
      starts <- random.shuffleList(slotStarts)
      durations <- starts.traverse(_ => random.elementOf(durationsMinutes))
      candidates = starts
        .zip(durations)
        .map((start, minutes) => (start, start.plusMinutes(minutes.toLong)))
      times = nonOverlapping(candidates, count)
      appointments <- times.traverse { (start, end) =>
        IO.randomUUID.map(id =>
          Appointment(
            AppointmentId(id),
            therapist.therapistId,
            TimeRange(timestamp(day, start), timestamp(day, end))
          )
        )
      }
    yield appointments

  /** Keeps candidates in order, skipping any that ends after 18:00 or overlaps
    * an already kept one, until `count` are kept. Result sorted by start time.
    */
  private def nonOverlapping(
      candidates: List[(LocalTime, LocalTime)],
      count: Int
  ): List[(LocalTime, LocalTime)] =
    candidates
      .filterNot((_, end) => end.isAfter(dayEnd))
      .foldLeft(List.empty[(LocalTime, LocalTime)]) {
        case (kept, (start, end)) =>
          val overlaps = kept.exists((keptStart, keptEnd) =>
            start.isBefore(keptEnd) && keptStart.isBefore(end)
          )
          if kept.size >= count || overlaps then kept else (start, end) :: kept
      }
      .sortBy((start, _) => start.toSecondOfDay)

  private def timestamp(day: LocalDate, time: LocalTime): Timestamp =
    Timestamp.fromOffsetDateTime(
      ZonedDateTime.of(day, time, zone).toOffsetDateTime
    )
