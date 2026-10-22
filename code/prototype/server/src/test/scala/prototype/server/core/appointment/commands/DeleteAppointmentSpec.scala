package prototype.server.core.appointment.commands

import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.patient.domain.PatientScopes
import prototype.server.core.therapist.domain.TherapistScopes
import prototype.server.support.HandlerWorld
import prototype.server.support.HandlerWorld.patient1

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `DeleteAppointment` handler: it reads only the appointment's own stream,
  * and must still reach the streams of the people in the appointment now, so
  * that they are free again.
  */
class DeleteAppointmentSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private def create(n: Int, therapists: List[String]) =
    CreateAppointment(appointmentId(n), therapists, List(patient1), "INDIVIDUAL", range(10, 11))

  "DeleteAppointment" should "be saved in the streams of the people in the appointment" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- world.runtime.execute(CreateAppointment.Handler, create(1, List("abc")))
        result <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(1)))
      yield result
    test.asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(List(AppointmentDeleted(appointmentId(1))))
      result.map(_.map(_.metadata.tags)) shouldBe Right(
        List(
          Set(
            AppointmentScopes.Appointment(appointmentId(1)).toTag,
            TherapistScopes.Therapist("abc").toTag,
            PatientScopes.Patient(patient1).toTag
          )
        )
      )
    }
  }

  it should "free the people of the appointment" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- world.runtime.execute(CreateAppointment.Handler, create(1, List("abc")))
        _ <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(1)))
        again <- world.runtime.execute(CreateAppointment.Handler, create(2, List("abc")))
      yield again
    test.asserting(_.isRight shouldBe true)
  }

  it should "free the people an update put in it, not the ones it had at first" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- world.runtime.execute(CreateAppointment.Handler, create(1, List("abc")))
        _ <- world.runtime.execute(
          UpdateAppointment.Handler,
          UpdateAppointment(appointmentId(1), List("xyz"), Nil, "INDIVIDUAL", range(10, 11))
        )
        result <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(1)))
        xyzFree <- world.runtime.execute(
          CreateAppointment.Handler,
          CreateAppointment(appointmentId(2), List("xyz"), Nil, "INDIVIDUAL", range(10, 11))
        )
      yield (result.map(_.flatMap(_.metadata.tags)), xyzFree.isRight)
    test.asserting { case (tags, xyzFree) =>
      tags.map(_.toSet) shouldBe Right(
        Set(
          AppointmentScopes.Appointment(appointmentId(1)).toTag,
          TherapistScopes.Therapist("xyz").toTag
        )
      )
      xyzFree shouldBe true
    }
  }

  it should "refuse an unknown appointment, and one deleted already" in {
    val test =
      for
        world <- HandlerWorld.make
        unknown <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(9)))
        _ <- world.runtime.execute(CreateAppointment.Handler, create(1, List("abc")))
        _ <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(1)))
        twice <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointmentId(1)))
      yield (unknown, twice)
    test.asserting(
      _ shouldBe (
        Left(APPOINTMENT_NOT_FOUND(appointmentId(9))),
        Left(APPOINTMENT_NOT_FOUND(appointmentId(1)))
      )
    )
  }
