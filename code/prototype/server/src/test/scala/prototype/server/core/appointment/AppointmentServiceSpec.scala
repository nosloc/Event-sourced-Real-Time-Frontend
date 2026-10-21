// DISABLED 2026-10-21 (Therapist moved to P4S): this file still targets the old
// in-memory therapist service (TherapistLookup). Re-enable it once the
// appointment and dashboard services depend on a trait the tests can stub.
// Tracked in `todo`.
/*
package prototype.server.core.appointment

import java.util.UUID

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.core.patient.PatientServiceImpl
import prototype.server.core.therapist.TherapistLookup
import prototype.server.eventStore.InMemoryEventStore

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
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
  private class StubTherapists(known: Set[String]) extends TherapistLookup:
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

  // Sends an appointment as the new version of itself: a test passes
  // `appt.copy(...)` with only what it changes
  private def update(service: AppointmentService[IO], edited: Appointment) =
    service.updateAppointment(
      edited.appointmentId,
      edited.appointmentType,
      edited.therapistIds,
      edited.patientIds,
      edited.timeRange
    )

  "updateAppointment" should "change the time range and keep the participants and the type" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(
          List(tst, arb), List(ann), AppointmentType.COUPLE, range(9, 10)
        )
        updated <- update(service, created.appointment.copy(timeRange = range(14, 15)))
        listed <- service.listTherapistAppointments(arb)
      yield (created, updated, listed)

    test.asserting { case (created, updated, listed) =>
      updated.appointment shouldBe created.appointment.copy(timeRange = range(14, 15))
      listed.appointments shouldBe List(updated.appointment)
    }
  }

  it should "change the type and keep the time range and the participants" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst, arb), List(ann), range(9, 10))
        updated <- update(service, created.appointment.copy(appointmentType = AppointmentType.GROUP))
        got <- service.listTherapistAppointments(tst)
      yield (created, updated, got)

    test.asserting { case (created, updated, got) =>
      updated.appointment shouldBe created.appointment.copy(appointmentType = AppointmentType.GROUP)
      got.appointments shouldBe List(updated.appointment)
    }
  }

  it should "change the participants: a new therapist sees it, a removed one no longer does" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        updated <- update(service, created.appointment.copy(therapistIds = List(arb), patientIds = List(bob)))
        forTst <- service.listTherapistAppointments(tst)
        forArb <- service.listTherapistAppointments(arb)
      yield (updated, forTst, forArb)

    test.asserting { case (updated, forTst, forArb) =>
      updated.appointment.therapistIds shouldBe List(arb)
      updated.appointment.patientIds shouldBe List(bob)
      forTst.appointments shouldBe Nil
      forArb.appointments shouldBe List(updated.appointment)
    }
  }

  it should "accept removing every patient" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        updated <- update(service, created.appointment.copy(patientIds = Nil))
      yield updated

    test.asserting(_.appointment.patientIds shouldBe Nil)
  }

  it should "fail with InvalidParticipants when there is no therapist, and change nothing" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- update(service, created.appointment.copy(therapistIds = Nil)).attempt
        got <- service.getAppointment(created.appointment.appointmentId)
      yield (created, result, got)

    test.asserting { case (created, result, got) =>
      result shouldBe Left(InvalidParticipants())
      got.appointment shouldBe created.appointment
    }
  }

  it should "fail with TherapistsNotFound, naming the unknown therapists" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- update(service, created.appointment.copy(therapistIds = List(tst, zzz))).attempt
      yield result

    test.asserting(_ shouldBe Left(TherapistsNotFound(List(zzz))))
  }

  it should "fail with PatientsNotFound, naming the unknown patients" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- update(service, created.appointment.copy(patientIds = List(ann, ghost))).attempt
      yield result

    test.asserting(_ shouldBe Left(PatientsNotFound(List(ghost))))
  }

  it should "fail with TherapistBusy when an added therapist is booked at that time, and change nothing" in {
    val test =
      for
        service <- newService
        mine <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(arb), List(bob), range(9, 10))
        clash <- update(service, mine.appointment.copy(therapistIds = List(tst, arb))).attempt
        got <- service.getAppointment(mine.appointment.appointmentId)
      yield (mine, clash, got)

    test.asserting { case (mine, clash, got) =>
      clash shouldBe Left(TherapistBusy(List(arb)))
      got.appointment shouldBe mine.appointment
    }
  }

  it should "fail with PatientBusy when an added patient is booked at that time" in {
    val test =
      for
        service <- newService
        mine <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(arb), List(bob), range(9, 10))
        clash <- update(service, mine.appointment.copy(patientIds = List(ann, bob))).attempt
      yield clash

    test.asserting(_ shouldBe Left(PatientBusy(List(bob))))
  }

  it should "accept its own current range (no overlap with itself)" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 11))
        result <- update(service, created.appointment).attempt
      yield result.map(_.appointment.timeRange)

    test.asserting(_ shouldBe Right(range(9, 11)))
  }

  it should "accept a range that overlaps only its own old range" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 11))
        result <- update(service, created.appointment.copy(timeRange = range(10, 12))).attempt
      yield result.map(_.appointment.timeRange)

    test.asserting(_ shouldBe Right(range(10, 12)))
  }

  it should "fail with TherapistBusy and change nothing when a therapist is booked in the new range" in {
    val test =
      for
        service <- newService
        first <- book(service, List(tst), List(ann), range(9, 10))
        _ <- book(service, List(tst), List(bob), range(14, 15))
        clash <- update(service, first.appointment.copy(timeRange = range(14, 16))).attempt
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
        clash <- update(service, first.appointment.copy(timeRange = range(14, 16))).attempt
      yield clash

    test.asserting(_ shouldBe Left(PatientBusy(List(ann))))
  }

  it should "fail with AppointmentNotFound for an unknown id" in {
    val unknown = AppointmentId(UUID.randomUUID())
    val test = newService.flatMap(
      _.updateAppointment(unknown, kind, List(tst), List(ann), range(9, 10))
    )

    test.attempt.asserting(_ shouldBe Left(AppointmentNotFound()))
  }

  it should "fail with InvalidTimeRange when the end is not after the start" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- update(service, created.appointment.copy(timeRange = range(11, 10))).attempt
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

  // --- prestations --------------------------------------------------------

  private def addPrestation(
      service: AppointmentService[IO],
      appt: Appointment,
      label: String = "Consultation",
      quantity: Int = 1,
      unitPrice: Float = 120.0f
  ) =
    service.createPrestation(appt.appointmentId, label, quantity, unitPrice)

  "createPrestation" should "add a prestation in status TO_BILL and return the appointment" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        added <- addPrestation(service, created.appointment, "Report", 2, 45.5f)
      yield added

    test.asserting { added =>
      val prestations = added.appointment.prestations
      prestations should have size 1
      prestations.head.label shouldBe "Report"
      prestations.head.quantity shouldBe 2
      prestations.head.unitPrice shouldBe 45.5f
      prestations.head.status shouldBe PrestationStatus.TO_BILL
      prestations.head.appointmentId shouldBe added.appointment.appointmentId
    }
  }

  it should "give every prestation its own id" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        _ <- addPrestation(service, created.appointment)
        second <- addPrestation(service, created.appointment)
      yield second.appointment.prestations.map(_.prestationId)

    test.asserting(ids => ids.distinct should have size 2)
  }

  it should "show the prestation when the appointment is read or listed" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        added <- addPrestation(service, created.appointment)
        got <- service.getAppointment(created.appointment.appointmentId)
        listed <- service.listTherapistAppointments(tst)
      yield (added, got, listed)

    test.asserting { case (added, got, listed) =>
      got.appointment shouldBe added.appointment
      listed.appointments shouldBe List(added.appointment)
    }
  }

  it should "fail with AppointmentNotFound for an unknown appointment" in {
    val unknown = AppointmentId(UUID.randomUUID())
    val test = newService.flatMap(_.createPrestation(unknown, "Consultation", 1, 120.0f))

    test.attempt.asserting(_ shouldBe Left(AppointmentNotFound()))
  }

  it should "fail with InvalidPrestation for an invalid label, quantity or price, and add nothing" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        blank <- addPrestation(service, created.appointment, label = " ").attempt
        noQuantity <- addPrestation(service, created.appointment, quantity = 0).attempt
        negative <- addPrestation(service, created.appointment, unitPrice = -1.0f).attempt
        got <- service.getAppointment(created.appointment.appointmentId)
      yield (blank, noQuantity, negative, got)

    test.asserting { case (blank, noQuantity, negative, got) =>
      blank shouldBe Left(InvalidPrestation())
      noQuantity shouldBe Left(InvalidPrestation())
      negative shouldBe Left(InvalidPrestation())
      got.appointment.prestations shouldBe Nil
    }
  }

  "updatePrestationStatus" should "move a prestation forward and return the appointment" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        added <- addPrestation(service, created.appointment)
        id = added.appointment.prestations.head.prestationId
        billed <- service.updatePrestationStatus(created.appointment.appointmentId, id, PrestationStatus.BILLED)
        paid <- service.updatePrestationStatus(created.appointment.appointmentId, id, PrestationStatus.PAID)
      yield (billed, paid)

    test.asserting { case (billed, paid) =>
      billed.appointment.prestations.map(_.status) shouldBe List(PrestationStatus.BILLED)
      paid.appointment.prestations.map(_.status) shouldBe List(PrestationStatus.PAID)
    }
  }

  it should "change only the named prestation" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        _ <- addPrestation(service, created.appointment, "First")
        two <- addPrestation(service, created.appointment, "Second")
        firstId = two.appointment.prestations.head.prestationId
        changed <- service.updatePrestationStatus(created.appointment.appointmentId, firstId, PrestationStatus.CANCELLED)
      yield changed

    test.asserting { changed =>
      changed.appointment.prestations.map(p => (p.label, p.status)) shouldBe
        List(("First", PrestationStatus.CANCELLED), ("Second", PrestationStatus.TO_BILL))
    }
  }

  it should "fail with InvalidStatusChange and change nothing when the change is not allowed" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        added <- addPrestation(service, created.appointment)
        id = added.appointment.prestations.head.prestationId
        jump <- service.updatePrestationStatus(created.appointment.appointmentId, id, PrestationStatus.PAID).attempt
        got <- service.getAppointment(created.appointment.appointmentId)
      yield (jump, got)

    test.asserting { case (jump, got) =>
      jump shouldBe Left(InvalidStatusChange())
      got.appointment.prestations.map(_.status) shouldBe List(PrestationStatus.TO_BILL)
    }
  }

  it should "refuse any change once a prestation is PAID or CANCELLED" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        paidOne <- addPrestation(service, created.appointment, "Paid")
        cancelledOne <- addPrestation(service, created.appointment, "Cancelled")
        apptId = created.appointment.appointmentId
        paidId = paidOne.appointment.prestations.head.prestationId
        cancelledId = cancelledOne.appointment.prestations.last.prestationId
        _ <- service.updatePrestationStatus(apptId, paidId, PrestationStatus.BILLED)
        _ <- service.updatePrestationStatus(apptId, paidId, PrestationStatus.PAID)
        _ <- service.updatePrestationStatus(apptId, cancelledId, PrestationStatus.CANCELLED)
        fromPaid <- service.updatePrestationStatus(apptId, paidId, PrestationStatus.CANCELLED).attempt
        fromCancelled <- service.updatePrestationStatus(apptId, cancelledId, PrestationStatus.BILLED).attempt
      yield (fromPaid, fromCancelled)

    test.asserting { case (fromPaid, fromCancelled) =>
      fromPaid shouldBe Left(InvalidStatusChange())
      fromCancelled shouldBe Left(InvalidStatusChange())
    }
  }

  it should "fail with PrestationNotFound for an unknown prestation" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- service
          .updatePrestationStatus(created.appointment.appointmentId, PrestationId(UUID.randomUUID()), PrestationStatus.BILLED)
          .attempt
      yield result

    test.asserting(_ shouldBe Left(PrestationNotFound()))
  }

  it should "fail with AppointmentNotFound for an unknown appointment" in {
    val test = newService.flatMap(
      _.updatePrestationStatus(AppointmentId(UUID.randomUUID()), PrestationId(UUID.randomUUID()), PrestationStatus.BILLED)
    )

    test.attempt.asserting(_ shouldBe Left(AppointmentNotFound()))
  }

  "deletePrestation" should "remove the prestation, whatever its status" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        apptId = created.appointment.appointmentId
        toBill <- addPrestation(service, created.appointment, "To bill")
        paidOne <- addPrestation(service, created.appointment, "Paid")
        paidId = paidOne.appointment.prestations.last.prestationId
        _ <- service.updatePrestationStatus(apptId, paidId, PrestationStatus.BILLED)
        _ <- service.updatePrestationStatus(apptId, paidId, PrestationStatus.PAID)
        _ <- service.deletePrestation(apptId, paidId)
        _ <- service.deletePrestation(apptId, toBill.appointment.prestations.head.prestationId)
        got <- service.getAppointment(apptId)
      yield got

    test.asserting(_.appointment.prestations shouldBe Nil)
  }

  it should "fail with PrestationNotFound for an unknown prestation" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        result <- service
          .deletePrestation(created.appointment.appointmentId, PrestationId(UUID.randomUUID()))
          .attempt
      yield result

    test.asserting(_ shouldBe Left(PrestationNotFound()))
  }

  it should "fail with AppointmentNotFound for an unknown appointment" in {
    val test = newService.flatMap(
      _.deletePrestation(AppointmentId(UUID.randomUUID()), PrestationId(UUID.randomUUID()))
    )

    test.attempt.asserting(_ shouldBe Left(AppointmentNotFound()))
  }

  "an appointment with prestations" should "keep them when it is updated" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        added <- addPrestation(service, created.appointment)
        updated <- update(service, added.appointment.copy(timeRange = range(14, 15)))
      yield (added, updated)

    test.asserting { case (added, updated) =>
      updated.appointment.timeRange shouldBe range(14, 15)
      updated.appointment.prestations shouldBe added.appointment.prestations
    }
  }

  it should "be deletable" in {
    val test =
      for
        service <- newService
        created <- book(service, List(tst), List(ann), range(9, 10))
        _ <- addPrestation(service, created.appointment)
        _ <- service.deleteAppointment(created.appointment.appointmentId)
        gone <- service.getAppointment(created.appointment.appointmentId).attempt
      yield gone

    test.asserting(_ shouldBe Left(AppointmentNotFound()))
  }
*/
