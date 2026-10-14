package prototype.server.core.appointment

import java.util.UUID
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.eventStore.InMemoryEventStore
import smithy4s.time.Timestamp

/** The appointment rules as a client of the service sees them, end to end:
  * service, handler, decider, viewer and the in-memory store are all real. Only
  * the therapist service is a stub, because the appointment service only needs
  * it to answer "does this therapist exist?".
  */
class AppointmentServiceSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers:

  private val tst = TherapistId("tst")
  private val arb = TherapistId("arb")

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  /** A therapist service that knows only the given trigrams. */
  private class StubTherapists(known: Set[String]) extends TherapistService[IO]:
    override def getTherapist(therapistId: TherapistId): IO[GetTherapistOutput] =
      if known.contains(therapistId.value) then
        IO.pure(
          GetTherapistOutput(
            Therapist(
              "First",
              "Last",
              therapistId,
              CompanyId(UUID.randomUUID()),
              Role.PSYCHOLOGIST
            )
          )
        )
      else IO.raiseError(TherapistNotFound())

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

  // A fresh service (and store) per test
  private val newService: IO[AppointmentService[IO]] =
    for
      store <- InMemoryEventStore.create
      service <- AppointmentServiceImpl(store, new StubTherapists(Set("tst", "arb")))
    yield service

  private def timeRangesOf(service: AppointmentService[IO], who: TherapistId) =
    service.listTherapistAppointments(who).map(_.appointments.map(_.timeRange))

  // --- create -------------------------------------------------------------

  "createAppointment" should "return the appointment and list it for its therapist" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(tst, range(9, 10))
        listed <- service.listTherapistAppointments(tst)
      yield (created, listed)

    test.asserting { case (created, listed) =>
      created.appointment.therapistId shouldBe tst
      created.appointment.timeRange shouldBe range(9, 10)
      listed.appointments shouldBe List(created.appointment)
    }
  }

  it should "fail with TherapistNotFound for an unknown therapist" in {
    val test = newService.flatMap(_.createAppointment(TherapistId("zzz"), range(9, 10)))

    test.attempt.asserting(_ shouldBe Left(TherapistNotFound()))
  }

  it should "fail with InvalidTimeRange when the end is not after the start" in {
    val test = newService.flatMap(_.createAppointment(tst, range(10, 9)))

    test.attempt.asserting(_ shouldBe Left(InvalidTimeRange()))
  }

  it should "fail with OverlappingAppointment when the therapist is busy" in {
    val test =
      for
        service <- newService
        _ <- service.createAppointment(tst, range(9, 11))
        clash <- service.createAppointment(tst, range(10, 12)).attempt
        listed <- timeRangesOf(service, tst)
      yield (clash, listed)

    test.asserting { case (clash, listed) =>
      clash shouldBe Left(OverlappingAppointment())
      // The rejected appointment was not stored
      listed shouldBe List(range(9, 11))
    }
  }

  it should "accept back-to-back appointments" in {
    val test =
      for
        service <- newService
        _ <- service.createAppointment(tst, range(9, 10))
        _ <- service.createAppointment(tst, range(10, 11))
        listed <- timeRangesOf(service, tst)
      yield listed

    test.asserting(_ should contain theSameElementsAs List(range(9, 10), range(10, 11)))
  }

  it should "accept the same time for a different therapist" in {
    val test =
      for
        service <- newService
        _ <- service.createAppointment(tst, range(9, 10))
        _ <- service.createAppointment(arb, range(9, 10))
        listed <- timeRangesOf(service, arb)
      yield listed

    test.asserting(_ shouldBe List(range(9, 10)))
  }

  it should "report an invalid range, not an overlap, when both apply" in {
    // 10:00-10:00 sits inside 09:00-11:00, but the range itself is invalid
    val test =
      for
        service <- newService
        _ <- service.createAppointment(tst, range(9, 11))
        result <- service.createAppointment(tst, range(10, 10)).attempt
      yield result

    test.asserting(_ shouldBe Left(InvalidTimeRange()))
  }

  // --- update -------------------------------------------------------------

  "updateAppointment" should "change the time range and keep the therapist" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(tst, range(9, 10))
        updated <- service.updateAppointment(created.appointment.appointmentId, range(14, 15))
        listed <- service.listTherapistAppointments(tst)
      yield (updated, listed)

    test.asserting { case (updated, listed) =>
      updated.appointment.timeRange shouldBe range(14, 15)
      updated.appointment.therapistId shouldBe tst
      listed.appointments shouldBe List(updated.appointment)
    }
  }

  it should "accept its own current range (no overlap with itself)" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(tst, range(9, 11))
        result <- service.updateAppointment(created.appointment.appointmentId, range(9, 11)).attempt
      yield result.map(_.appointment.timeRange)

    test.asserting(_ shouldBe Right(range(9, 11)))
  }

  it should "accept a range that overlaps only its own old range" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(tst, range(9, 11))
        result <- service.updateAppointment(created.appointment.appointmentId, range(10, 12)).attempt
      yield result.map(_.appointment.timeRange)

    test.asserting(_ shouldBe Right(range(10, 12)))
  }

  it should "fail with OverlappingAppointment and change nothing when another appointment is in the way" in {
    val test =
      for
        service <- newService
        first <- service.createAppointment(tst, range(9, 10))
        _ <- service.createAppointment(tst, range(14, 15))
        clash <- service.updateAppointment(first.appointment.appointmentId, range(14, 16)).attempt
        listed <- timeRangesOf(service, tst)
      yield (clash, listed)

    test.asserting { case (clash, listed) =>
      clash shouldBe Left(OverlappingAppointment())
      listed should contain theSameElementsAs List(range(9, 10), range(14, 15))
    }
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
        created <- service.createAppointment(tst, range(9, 10))
        result <- service.updateAppointment(created.appointment.appointmentId, range(11, 10)).attempt
      yield result

    test.asserting(_ shouldBe Left(InvalidTimeRange()))
  }

  // --- delete -------------------------------------------------------------

  "deleteAppointment" should "remove the appointment and free its time" in {
    val test =
      for
        service <- newService
        created <- service.createAppointment(tst, range(9, 10))
        _ <- service.deleteAppointment(created.appointment.appointmentId)
        afterDelete <- service.listTherapistAppointments(tst)
        again <- service.createAppointment(tst, range(9, 10)).attempt
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
    val test = newService.flatMap(_.listTherapistAppointments(TherapistId("zzz")))

    test.attempt.asserting(_ shouldBe Left(TherapistNotFound()))
  }
