package prototype.server.core.appointment.commands

import java.util.UUID

import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.TimeRange
import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.patient.domain.PatientScopes
import prototype.server.core.therapist.domain.TherapistScopes
import prototype.server.support.HandlerWorld
import prototype.server.support.HandlerWorld.{patient1, patient2}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `UpdateAppointment` handler. Besides its own rules, the update must reach
  * every stream that a later creation reads: the people added get the whole
  * appointment in an event of their own, and the people removed see the update
  * and know they are free.
  */
class UpdateAppointmentSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private def create(n: Int, therapists: List[String], patients: List[UUID], timeRange: TimeRange) =
    CreateAppointment(appointmentId(n), therapists, patients, "INDIVIDUAL", timeRange)

  private def update(
      n: Int,
      therapists: List[String],
      patients: List[UUID],
      timeRange: TimeRange,
      apptType: String = "INDIVIDUAL"
  ) =
    UpdateAppointment(appointmentId(n), therapists, patients, apptType, timeRange)

  private def created(world: HandlerWorld, command: CreateAppointment) =
    world.runtime.execute(CreateAppointment.Handler, command)

  private def updated(world: HandlerWorld, command: UpdateAppointment) =
    world.runtime.execute(UpdateAppointment.Handler, command)

  "UpdateAppointment" should "save what changed, in the streams of the people who stay and leave, and the whole appointment for each person who joins" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc", "xyz"), List(patient1, patient2), range(10, 11)))
        result <- updated(world, update(1, List("abc", "qrs"), List(patient2), range(14, 15), "COUPLE"))
      yield result

    test.asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(
        List(
          AppointmentUpdated(
            appointmentId(1),
            apptType = Some("COUPLE"),
            startTime = Some(millis(14)),
            endTime = Some(millis(15)),
            therapistsAdded = List("qrs"),
            therapistsRemoved = List("xyz"),
            patientsAdded = Nil,
            patientsRemoved = List(patient1)
          ),
          // The appointment as it is after the update
          TherapistAddedToAppointment(
            appointmentId(1),
            "qrs",
            List("abc", "qrs"),
            List(patient2),
            "COUPLE",
            millis(14),
            millis(15)
          )
        )
      )
      result.map(_.map(_.metadata.tags)) shouldBe Right(
        List(
          // Who stays and who leaves, not who joins
          Set(
            AppointmentScopes.Appointment(appointmentId(1)).toTag,
            TherapistScopes.Therapist("abc").toTag,
            TherapistScopes.Therapist("xyz").toTag,
            PatientScopes.Patient(patient1).toTag,
            PatientScopes.Patient(patient2).toTag
          ),
          Set(TherapistScopes.Therapist("qrs").toTag)
        )
      )
    }
  }

  it should "make someone it adds busy for later creations" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
        _ <- updated(world, update(1, List("abc", "xyz"), Nil, range(10, 11)))
        later <- created(world, create(2, List("xyz"), Nil, TimeRange(at(10, 30), at(11, 30))))
      yield later
    test.asserting(_ shouldBe Left(THERAPIST_BUSY(List("xyz"))))
  }

  it should "free someone it removes, and keep the others busy" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), List(patient1), range(10, 11)))
        _ <- updated(world, update(1, List("xyz"), List(patient2), range(10, 11)))
        freedTherapist <- created(world, create(2, List("abc"), Nil, range(10, 11)))
        freedPatient <- created(world, create(3, List("qrs"), List(patient1), range(10, 11)))
        stillBusy <- created(world, create(4, List("xyz"), Nil, range(10, 11)))
      yield (freedTherapist.isRight, freedPatient.isRight, stillBusy)
    test.asserting(_ shouldBe (true, true, Left(THERAPIST_BUSY(List("xyz")))))
  }

  it should "move the busy time with the appointment" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
        _ <- updated(world, update(1, List("abc"), Nil, range(14, 15)))
        oldTime <- created(world, create(2, List("abc"), Nil, range(10, 11)))
        newTime <- created(world, create(3, List("abc"), Nil, TimeRange(at(14, 30), at(15, 30))))
      yield (oldTime.isRight, newTime)
    test.asserting(_ shouldBe (true, Left(THERAPIST_BUSY(List("abc")))))
  }

  // Someone added only has their "added" event, then the later updates
  it should "move the busy time of someone it added, when a later update moves the appointment" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
        _ <- updated(world, update(1, List("abc", "xyz"), Nil, range(10, 11)))
        _ <- updated(world, update(1, List("abc", "xyz"), Nil, range(14, 15)))
        oldTime <- created(world, create(2, List("xyz"), Nil, range(10, 11)))
        newTime <- created(world, create(3, List("xyz"), Nil, TimeRange(at(14, 30), at(15, 30))))
      yield (oldTime.isRight, newTime)
    test.asserting(_ shouldBe (true, Left(THERAPIST_BUSY(List("xyz")))))
  }

  it should "make someone it adds busy at the new time, when the same update moves the appointment" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
        _ <- updated(world, update(1, List("abc"), List(patient1), range(14, 15)))
        oldTime <- created(world, create(2, List("xyz"), List(patient1), range(10, 11)))
        newTime <- created(world, create(3, List("xyz"), List(patient1), TimeRange(at(14, 30), at(15, 30))))
      yield (oldTime.isRight, newTime)
    test.asserting(_ shouldBe (true, Left(PATIENT_BUSY(List(patient1)))))
  }

  it should "make someone busy again, at the current time, when it adds them back" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc", "xyz"), Nil, range(10, 11)))
        _ <- updated(world, update(1, List("abc"), Nil, range(10, 11)))
        // Moved while xyz is out: xyz's stream does not see this one
        _ <- updated(world, update(1, List("abc"), Nil, range(14, 15)))
        _ <- updated(world, update(1, List("abc", "xyz"), Nil, range(14, 15)))
        oldTime <- created(world, create(2, List("xyz"), Nil, range(10, 11)))
        newTime <- created(world, create(3, List("xyz"), Nil, TimeRange(at(14, 30), at(15, 30))))
      yield (oldTime.isRight, newTime)
    test.asserting(_ shouldBe (true, Left(THERAPIST_BUSY(List("xyz")))))
  }

  it should "not clash with its own old time" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), List(patient1), range(10, 11)))
        result <- updated(world, update(1, List("abc"), List(patient1), TimeRange(at(10, 30), at(11, 30))))
      yield result
    test.asserting(_.isRight shouldBe true)
  }

  it should "refuse a time that clashes with another appointment of its therapist" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
        _ <- created(world, create(2, List("abc"), Nil, range(14, 15)))
        result <- updated(world, update(2, List("abc"), Nil, TimeRange(at(10, 30), at(11, 30))))
      yield result
    test.asserting(_ shouldBe Left(THERAPIST_BUSY(List("abc"))))
  }

  it should "save nothing when nothing changes, whatever the order of the people" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc", "xyz"), List(patient1), range(10, 11)))
        result <- updated(world, update(1, List("xyz", "abc"), List(patient1), range(10, 11)))
      yield result
    test.asserting(_ shouldBe Right(Nil))
  }

  it should "accept removing every patient" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), List(patient1, patient2), range(10, 11)))
        result <- updated(world, update(1, List("abc"), Nil, range(10, 11)))
      yield result
    test.asserting(_.map(_.map(_.payload)) shouldBe Right(
      List(
        AppointmentUpdated(
          appointmentId(1),
          apptType = None,
          startTime = None,
          endTime = None,
          therapistsAdded = Nil,
          therapistsRemoved = Nil,
          patientsAdded = Nil,
          patientsRemoved = List(patient1, patient2).sortBy(_.toString)
        )
      )
    ))
  }

  it should "refuse a patient it adds who is booked elsewhere at that time" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), List(patient1), range(10, 11)))
        _ <- created(world, create(2, List("xyz"), List(patient2), range(10, 11)))
        result <- updated(world, update(2, List("xyz"), List(patient2, patient1), range(10, 11)))
      yield result
    test.asserting(_ shouldBe Left(PATIENT_BUSY(List(patient1))))
  }

  private val refused = List(
    ("an unknown appointment", update(9, List("abc"), Nil, range(10, 11)), APPOINTMENT_NOT_FOUND(appointmentId(9))),
    ("no therapist", update(1, Nil, Nil, range(10, 11)), NO_THERAPIST(appointmentId(1))),
    ("a range that ends before it starts", update(1, List("abc"), Nil, range(12, 11)), INVALID_TIME_RANGE(appointmentId(1))),
    ("a range that ends when it starts", update(1, List("abc"), Nil, range(11, 11)), INVALID_TIME_RANGE(appointmentId(1))),
    ("an unknown therapist", update(1, List("abc", "nope"), Nil, range(10, 11)), THERAPISTS_NOT_FOUND(List("nope"))),
    (
      "an unknown patient",
      update(1, List("abc"), List(appointmentId(99)), range(10, 11)),
      PATIENTS_NOT_FOUND(List(appointmentId(99)))
    )
  )

  refused.foreach { (description, command, error) =>
    it should s"refuse $description without saving an event" in {
      val test =
        for
          world <- HandlerWorld.make
          _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
          before <- world.store.all
          result <- updated(world, command)
          after <- world.store.all
        yield (result, after.size - before.size)
      test.asserting { case (result, added) =>
        result shouldBe Left(error)
        added shouldBe 0
      }
    }
  }

  it should "refuse an appointment that was deleted" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- created(world, create(1, List("abc"), Nil, range(10, 11)))
        _ <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(1)))
        result <- updated(world, update(1, List("abc"), Nil, range(12, 13)))
      yield result
    test.asserting(_ shouldBe Left(APPOINTMENT_NOT_FOUND(appointmentId(1))))
  }
