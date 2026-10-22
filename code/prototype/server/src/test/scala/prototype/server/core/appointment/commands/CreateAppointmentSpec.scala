package prototype.server.core.appointment.commands

import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

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

/** The `CreateAppointment` handler on an in-memory event store holding a
  * company, three therapists and two patients: the rules, the saved event and
  * the streams it is put in, since later commands find it there.
  */
class CreateAppointmentSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private def create(
      n: Int,
      therapists: List[String],
      patients: List[UUID],
      timeRange: TimeRange
  ) =
    CreateAppointment(appointmentId(n), therapists, patients, "INDIVIDUAL", timeRange)

  private def run(world: HandlerWorld, command: CreateAppointment) =
    world.runtime.execute(CreateAppointment.Handler, command)

  "CreateAppointment" should "save the appointment in its stream and in each participant's stream" in {
    val test =
      for
        world <- HandlerWorld.make
        result <- run(world, create(1, List("abc"), List(patient1), range(10, 11)))
      yield result

    test.asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(
        List(
          AppointmentCreated(
            appointmentId(1),
            List("abc"),
            List(patient1),
            "INDIVIDUAL",
            millis(10),
            millis(11)
          )
        )
      )
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

  it should "accept appointments that only touch at their ends" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- run(world, create(1, List("abc"), Nil, range(10, 11)))
        second <- run(world, create(2, List("abc"), Nil, range(11, 12)))
      yield second
    test.asserting(_.isRight shouldBe true)
  }

  it should "accept other people at the same time" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- run(world, create(1, List("abc"), List(patient1), range(10, 11)))
        second <- run(world, create(2, List("xyz"), List(patient2), range(10, 11)))
      yield second
    test.asserting(_.isRight shouldBe true)
  }

  it should "accept an appointment with no patient" in {
    HandlerWorld.make
      .flatMap(run(_, create(1, List("abc"), Nil, range(10, 11))))
      .asserting(_.isRight shouldBe true)
  }

  it should "accept the same person listed twice" in {
    HandlerWorld.make
      .flatMap(run(_, create(1, List("abc", "abc"), List(patient1, patient1), range(10, 11))))
      .asserting(_.isRight shouldBe true)
  }

  // The range is checked before the busy people: an empty range clashes with
  // nothing, so naming busy people would mislead
  it should "report an invalid range, not a busy therapist, when both apply" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- run(world, create(1, List("abc"), Nil, range(10, 12)))
        second <- run(world, create(2, List("abc"), Nil, range(11, 11)))
      yield second
    test.asserting(_ shouldBe Left(INVALID_TIME_RANGE(appointmentId(2))))
  }

  private val refused = List(
    ("no therapist", create(1, Nil, Nil, range(10, 11)), NO_THERAPIST(appointmentId(1))),
    (
      "a range that ends before it starts",
      create(1, List("abc"), Nil, range(12, 11)),
      INVALID_TIME_RANGE(appointmentId(1))
    ),
    (
      "a range that ends when it starts",
      create(1, List("abc"), Nil, range(11, 11)),
      INVALID_TIME_RANGE(appointmentId(1))
    ),
    (
      "an unknown therapist, named once and in order",
      create(1, List("abc", "nope", "nada", "nope"), Nil, range(10, 11)),
      THERAPISTS_NOT_FOUND(List("nope", "nada"))
    ),
    (
      "an unknown patient",
      create(
        1,
        List("abc"),
        List(patient1, appointmentId(99)),
        range(10, 11)
      ),
      PATIENTS_NOT_FOUND(List(appointmentId(99)))
    )
  )

  refused.foreach { (description, command, error) =>
    it should s"refuse $description without saving an event" in {
      val test =
        for
          world <- HandlerWorld.make
          before <- world.store.all
          result <- run(world, command)
          after <- world.store.all
        yield (result, after.size - before.size)
      test.asserting { case (result, added) =>
        result shouldBe Left(error)
        added shouldBe 0
      }
    }
  }

  it should "refuse a taken id" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- run(world, create(1, List("abc"), Nil, range(10, 11)))
        second <- run(world, create(1, List("xyz"), Nil, range(14, 15)))
      yield second
    test.asserting(_ shouldBe Left(APPOINTMENT_ALREADY_EXISTS(appointmentId(1))))
  }

  it should "refuse a busy therapist, naming only the busy ones" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- run(world, create(1, List("abc"), Nil, range(10, 12)))
        second <- run(world, create(2, List("xyz", "abc"), Nil, range(11, 13)))
      yield second
    test.asserting(_ shouldBe Left(THERAPIST_BUSY(List("abc"))))
  }

  it should "refuse a busy patient" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- run(world, create(1, List("abc"), List(patient1), range(10, 12)))
        second <- run(
          world,
          create(2, List("xyz"), List(patient2, patient1), range(11, 13))
        )
      yield second
    test.asserting(_ shouldBe Left(PATIENT_BUSY(List(patient1))))
  }

  it should "accept only one of two overlapping creations for the same therapist at the same moment" in {
    val test =
      for
        world <- HandlerWorld.make
        results <- (
          run(world, create(1, List("abc"), Nil, range(10, 12))),
          run(world, create(2, List("abc"), Nil, range(11, 13)))
        ).parTupled
      yield results
    test.asserting { case (first, second) =>
      List(first, second).count(_.isRight) shouldBe 1
      List(first, second).collect { case Left(error) => error } shouldBe
        List(THERAPIST_BUSY(List("abc")))
    }
  }
