package prototype.server.core.appointment

import java.util.UUID
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.patient.PatientServiceImpl
import prototype.server.core.therapist.TherapistServiceImpl
import prototype.server.eventStore.InMemoryEventStore
import smithy4s.time.{LocalDate, Timestamp}

/** The appointment rules as a client of the service sees them, end to end:
  * service, handler, decider, viewer and the in-memory store are all real. Only
  * the therapist and patient services are stubs, because the appointment
  * service only needs them to answer "do these people exist?".
  */
class AppointmentServiceSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers:

  private val tst = TherapistId("tst")
  private val arb = TherapistId("arb")
  private val zzz = TherapistId("zzz") // unknown to the stub

  private val ann = PatientId(UUID.fromString("00000000-0000-4000-8000-000000000001"))
  private val bob = PatientId(UUID.fromString("00000000-0000-4000-8000-000000000002"))
  private val ghost = PatientId(UUID.fromString("00000000-0000-4000-8000-0000000000ff")) // unknown

  private val kind = AppointmentType.INDIVIDUAL

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  /** A therapist service that knows only the given trigrams. */
  private class StubTherapists(known: Set[String]) extends TherapistServiceImpl:
    private def therapist(id: TherapistId) =
      Therapist("First", "Last", id, CompanyId(UUID.randomUUID()), Role.PSYCHOLOGIST)

    override def getTherapist(therapistId: TherapistId): IO[GetTherapistOutput] =
      if known.contains(therapistId.value) then
        IO.pure(GetTherapistOutput(therapist(therapistId)))
      else IO.raiseError(TherapistNotFound(therapistId))

    override def getTherapists(ids: List[TherapistId]): IO[List[Therapist]] =
      IO.pure(ids.distinct.filter(id => known.contains(id.value)).map(therapist))

    override def listTherapists(): IO[ListTherapistsOutput] =
      IO.raiseError(new NotImplementedError("not used by these tests"))

    override def createTherapist(
        firstName: String,
        lastName: String,
        therapistId: TherapistId,
        companyId: CompanyId,
        role: Role
    ): IO[CreateTherapistOutput] =
      IO.raiseError(new NotImplementedError("not used by these tests"))

  /** A patient service that knows only the given ids. */
  private class StubPatients(known: Set[PatientId]) extends PatientServiceImpl:
    private def patient(id: PatientId) =
      Patient(id, "First", "Last", LocalDate(1990, 1, 1))

    override def getPatient(patientId: PatientId): IO[GetPatientOutput] =
      if known.contains(patientId) then
        IO.pure(GetPatientOutput(patient(patientId)))
      else IO.raiseError(PatientNotFound(patientId))

    override def getPatients(ids: List[PatientId]): IO[List[Patient]] =
      IO.pure(ids.distinct.filter(known.contains).map(patient))

    override def listPatients(): IO[ListPatientsOutput] =
      IO.raiseError(new NotImplementedError("not used by these tests"))

    override def createPatient(
        firstName: String,
        lastName: String,
        dateOfBirth: LocalDate
    ): IO[CreatePatientOutput] =
      IO.raiseError(new NotImplementedError("not used by these tests"))

  // A fresh service (and store) per test
  private val newService: IO[AppointmentService[IO]] =
    for
      store <- InMemoryEventStore.create
      service <- AppointmentServiceImpl(
        store,
        new StubTherapists(Set("tst", "arb")),
        new StubPatients(Set(ann, bob))
      )
    yield service

  private def timeRangesOf(service: AppointmentService[IO], who: TherapistId) =
    service.listTherapistAppointments(who).map(_.appointments.map(_.timeRange))

  private def book(
      service: AppointmentService[IO],
      therapists: List[TherapistId],
      patients: List[PatientId],
      r: TimeRange
  ) = service.createAppointment(therapists, patients, kind, r)

  // --- create -------------------------------------------------------------

  "createAppointment" should "return the appointment and list it for each of its therapists" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(
          List(tst, arb), List(ann, bob), AppointmentType.FAMILY, range(9, 10)
        )
        listedTst <- service.listTherapistAppointments(tst)
        listedArb <- service.listTherapistAppointments(arb)
      yield (created, listedTst, listedArb)

    test.asserting { case (created, listedTst, listedArb) =>
      created.appointment.therapistIds shouldBe List(tst, arb)
      created.appointment.patientIds shouldBe List(ann, bob)
      created.appointment.appointmentType shouldBe AppointmentType.FAMILY
      created.appointment.timeRange shouldBe range(9, 10)
      listedTst.appointments shouldBe List(created.appointment)
      listedArb.appointments shouldBe List(created.appointment)
    }
  }

  it should "accept an appointment with no patient" in {
    val test = newService.flatMap(book(_, List(tst), Nil, range(9, 10)))

    test.asserting(_.appointment.patientIds shouldBe Nil)
  }

  it should "fail with InvalidParticipants when there is no therapist" in {
    val test = newService.flatMap(book(_, Nil, List(ann), range(9, 10)))

    test.attempt.asserting(_ shouldBe Left(InvalidParticipants()))
  }

  it should "fail with TherapistsNotFound, naming the unknown therapists" in {
    val test = newService.flatMap(book(_, List(tst, zzz), List(ann), range(9, 10)))

    test.attempt.asserting(_ shouldBe Left(TherapistsNotFound(List(zzz))))
  }

  it should "fail with PatientsNotFound, naming the unknown patients" in {
    val test = newService.flatMap(book(_, List(tst), List(ann, ghost), range(9, 10)))

    test.attempt.asserting(_ shouldBe Left(PatientsNotFound(List(ghost))))
  }

  it should "accept the same participant listed twice" in {
    val test = newService.flatMap(book(_, List(tst, tst), List(ann, ann), range(9, 10)))

    test.attempt.asserting(_.isRight shouldBe true)
  }

  it should "fail with InvalidTimeRange when the end is not after the start" in {
    val test = newService.flatMap(book(_, List(tst), List(ann), range(10, 9)))

    test.attempt.asserting(_ shouldBe Left(InvalidTimeRange()))
  }

  it should "fail with TherapistBusy, naming only the busy therapist, and store nothing" in {
    val test =
      for
        service <- newService
        _ <- book(service, List(tst), List(ann), range(9, 11))
        clash <- book(service, List(tst, arb), List(bob), range(10, 12)).attempt
        listed <- timeRangesOf(service, arb)
      yield (clash, listed)

    test.asserting { case (clash, listed) =>
      clash shouldBe Left(TherapistBusy(List(tst)))
      // The rejected appointment was not stored for the free therapist either
      listed shouldBe Nil
    }
  }

  it should "fail with PatientBusy, naming only the busy patient, when a patient is booked elsewhere" in {
    val test =
      for
        service <- newService
        _ <- book(service, List(tst), List(ann), range(9, 11))
        clash <- book(service, List(arb), List(ann, bob), range(10, 12)).attempt
      yield clash

    test.asserting(_ shouldBe Left(PatientBusy(List(ann))))
  }

  it should "accept back-to-back appointments" in {
    val test =
      for
        service <- newService
        _ <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(tst), List(ann), range(10, 11))
        listed <- timeRangesOf(service, tst)
      yield listed

    test.asserting(_ should contain theSameElementsAs List(range(9, 10), range(10, 11)))
  }

  it should "accept the same time for other therapists and other patients" in {
    val test =
      for
        service <- newService
        _ <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(arb), List(bob), range(9, 10))
        listed <- timeRangesOf(service, arb)
      yield listed

    test.asserting(_ shouldBe List(range(9, 10)))
  }

  it should "report an invalid range, not a busy participant, when both apply" in {
    // 10:00-10:00 sits inside 09:00-11:00, but the range itself is invalid
    val test =
      for
        service <- newService
        _ <- book(service, List(tst), List(ann), range(9, 11))
        result <- book(service, List(tst), List(ann), range(10, 10)).attempt
      yield result

    test.asserting(_ shouldBe Left(InvalidTimeRange()))
  }

  // --- update -------------------------------------------------------------

  "updateAppointment" should "change the time range and keep the participants and the type" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(
          List(tst, arb), List(ann), AppointmentType.COUPLE, range(9, 10)
        )
        updated <- service.updateAppointment(created.appointment.appointmentId, range(14, 15))
        listed <- service.listTherapistAppointments(arb)
      yield (created, updated, listed)

    test.asserting { case (created, updated, listed) =>
      updated.appointment shouldBe created.appointment.copy(timeRange = range(14, 15))
      listed.appointments shouldBe List(updated.appointment)
    }
  }

  it should "accept its own current range (no overlap with itself)" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 11))
        result <- service.updateAppointment(created.appointment.appointmentId, range(9, 11)).attempt
      yield result.map(_.appointment.timeRange)

    test.asserting(_ shouldBe Right(range(9, 11)))
  }

  it should "accept a range that overlaps only its own old range" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 11))
        result <- service.updateAppointment(created.appointment.appointmentId, range(10, 12)).attempt
      yield result.map(_.appointment.timeRange)

    test.asserting(_ shouldBe Right(range(10, 12)))
  }

  it should "fail with TherapistBusy and change nothing when a therapist is booked in the new range" in {
    val test =
      for
        service <- newService
        first <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(tst), List(bob), range(14, 15))
        clash <- service.updateAppointment(first.appointment.appointmentId, range(14, 16)).attempt
        listed <- timeRangesOf(service, tst)
      yield (clash, listed)

    test.asserting { case (clash, listed) =>
      clash shouldBe Left(TherapistBusy(List(tst)))
      listed should contain theSameElementsAs List(range(9, 10), range(14, 15))
    }
  }

  it should "fail with PatientBusy when a patient is booked in the new range with another therapist" in {
    val test =
      for
        service <- newService
        first <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(arb), List(ann), range(14, 15))
        clash <- service.updateAppointment(first.appointment.appointmentId, range(14, 16)).attempt
      yield clash

    test.asserting(_ shouldBe Left(PatientBusy(List(ann))))
  }

  it should "fail with AppointmentNotFound for an unknown id" in {
    val unknown = AppointmentId(UUID.randomUUID())
    val test = newService.flatMap(_.updateAppointment(unknown, range(9, 10)))

    test.attempt.asserting(_ shouldBe Left(AppointmentNotFound()))
  }

  it should "fail with InvalidTimeRange when the end is not after the start" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- service.updateAppointment(created.appointment.appointmentId, range(11, 10)).attempt
      yield result

    test.asserting(_ shouldBe Left(InvalidTimeRange()))
  }

  // --- delete -------------------------------------------------------------

  "deleteAppointment" should "remove the appointment and free the time of every participant" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst, arb), List(ann), range(9, 10))
        _ <- service.deleteAppointment(created.appointment.appointmentId)
        afterDelete <- service.listTherapistAppointments(arb)
        again <- book(service, List(tst), List(ann), range(9, 10)).attempt
      yield (afterDelete, again)

    test.asserting { case (afterDelete, again) =>
      afterDelete.appointments shouldBe Nil
      again.isRight shouldBe true
    }
  }

  it should "fail with AppointmentNotFound for an unknown id" in {
    val unknown = AppointmentId(UUID.randomUUID())
    val test = newService.flatMap(_.deleteAppointment(unknown))

    test.attempt.asserting(_ shouldBe Left(AppointmentNotFound()))
  }

  // --- list ---------------------------------------------------------------

  "listTherapistAppointments" should "fail with TherapistNotFound for an unknown therapist" in {
    val test = newService.flatMap(_.listTherapistAppointments(zzz))

    test.attempt.asserting(_ shouldBe Left(TherapistNotFound(zzz)))
  }

  it should "not list another therapist's appointments" in {
    val test =
      for
        service <- newService
        _ <- book(service, List(tst), List(ann), range(9, 10))
        listed <- service.listTherapistAppointments(arb)
      yield listed

    test.asserting(_.appointments shouldBe Nil)
  }
