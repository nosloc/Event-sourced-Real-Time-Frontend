package prototype.server.core.appointment.commands

import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

import prototype.api.PrestationStatus
import prototype.api.PrestationStatus.*
import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.support.HandlerWorld

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The three prestation commands of an appointment: create, change the status
  * and delete. Each reads the appointment's stream, which also holds the other
  * prestations of the appointment, so each must only look at its own.
  */
class PrestationCommandsSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val appointment = appointmentId(1)

  // An appointment with one prestation (prestationId(1)), at its first status
  private val withPrestation: IO[HandlerWorld] =
    for
      world <- HandlerWorld.make
      _ <- world.runtime
        .execute(
          CreateAppointment.Handler,
          CreateAppointment(appointment, List("abc"), Nil, "INDIVIDUAL", range(10, 11))
        )
        .flatMap(IO.fromEither)
      _ <- addPrestation(world, prestationId(1)).flatMap(IO.fromEither)
    yield world

  private def addPrestation(
      world: HandlerWorld,
      id: UUID,
      label: String = "Consultation",
      unitPrice: Float = 120f,
      quantity: Int = 1,
      to: UUID = appointment
  ) =
    world.runtime.execute(
      CreatePrestation.Handler,
      CreatePrestation(to, id, label, unitPrice, quantity)
    )

  private def setStatus(world: HandlerWorld, id: UUID, status: PrestationStatus, of: UUID = appointment) =
    world.runtime.execute(UpdatePrestationStatus.Handler, UpdatePrestationStatus(of, id, status))

  private def remove(world: HandlerWorld, id: UUID, of: UUID = appointment) =
    world.runtime.execute(DeletePrestation.Handler, DeletePrestation(of, id))

  // Creating

  "CreatePrestation" should "save the prestation in the appointment's and the prestation's streams" in {
    val test =
      for
        world <- HandlerWorld.make
        _ <- world.runtime.execute(
          CreateAppointment.Handler,
          CreateAppointment(appointment, List("abc"), Nil, "INDIVIDUAL", range(10, 11))
        )
        result <- addPrestation(world, prestationId(1), "Report", 80.5f, 2)
      yield result
    test.asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(
        List(PrestationCreated(appointment, prestationId(1), "Report", 80.5f, 2))
      )
      result.map(_.map(_.metadata.tags)) shouldBe Right(
        List(
          Set(
            AppointmentScopes.Appointment(appointment).toTag,
            AppointmentScopes.Prestation(prestationId(1)).toTag
          )
        )
      )
    }
  }

  private val invalid = List(
    ("a blank label", ("  ", 120f, 1)),
    ("a negative price", ("Consultation", -1f, 1)),
    ("a price that is not a number", ("Consultation", Float.NaN, 1)),
    ("an infinite price", ("Consultation", Float.PositiveInfinity, 1)),
    ("a quantity of zero", ("Consultation", 120f, 0))
  )

  invalid.foreach { case (description, (label, price, quantity)) =>
    it should s"refuse $description" in {
      val test =
        for
          world <- withPrestation
          result <- addPrestation(world, prestationId(2), label, price, quantity)
        yield result
      test.asserting(_ shouldBe Left(INVALID_PRESTATION(appointment)))
    }
  }

  it should "accept a price of zero" in {
    withPrestation
      .flatMap(addPrestation(_, prestationId(2), unitPrice = 0f))
      .asserting(_.isRight shouldBe true)
  }

  it should "refuse an unknown or deleted appointment" in {
    val test =
      for
        world <- withPrestation
        unknown <- addPrestation(world, prestationId(2), to = appointmentId(9))
        _ <- world.runtime.execute(DeleteAppointment.Handler, DeleteAppointment(appointment))
        deleted <- addPrestation(world, prestationId(3))
      yield (unknown, deleted)
    test.asserting(
      _ shouldBe (
        Left(APPOINTMENT_NOT_FOUND(appointmentId(9))),
        Left(APPOINTMENT_NOT_FOUND(appointment))
      )
    )
  }

  it should "refuse a prestation id that is taken" in {
    withPrestation
      .flatMap(addPrestation(_, prestationId(1)))
      .asserting(_ shouldBe Left(PRESTATION_ALREADY_EXISTS(appointment, prestationId(1))))
  }

  // Changing the status

  private val allowed = List(
    List(BILLED),
    List(BILLED, PAID),
    List(CANCELLED),
    List(BILLED, CANCELLED)
  )

  allowed.foreach { steps =>
    "UpdatePrestationStatus" should s"allow TO_BILL then ${steps.map(_.stringValue).mkString(" then ")}" in {
      val test =
        for
          world <- withPrestation
          results <- steps.traverse(setStatus(world, prestationId(1), _))
        yield results
      test.asserting(_.forall(_.isRight) shouldBe true)
    }
  }

  private val refusedChanges = List(
    ("skip a step", Nil, PAID),
    ("go back", List(BILLED), TO_BILL),
    ("stay where it is", Nil, TO_BILL),
    ("leave PAID", List(BILLED, PAID), CANCELLED),
    ("leave CANCELLED", List(CANCELLED), BILLED)
  )

  refusedChanges.foreach { (description, before, next) =>
    it should s"refuse to $description" in {
      val test =
        for
          world <- withPrestation
          _ <- before.traverse_(setStatus(world, prestationId(1), _).flatMap(IO.fromEither))
          result <- setStatus(world, prestationId(1), next)
        yield result
      test.asserting(_ shouldBe Left(INVALID_STATUS_CHANGE(appointment, prestationId(1))))
    }
  }

  it should "save the new status in the appointment's and the prestation's streams" in {
    val test =
      for
        world <- withPrestation
        result <- setStatus(world, prestationId(1), BILLED)
      yield result
    test.asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(
        List(PrestationStatusUpdated(appointment, prestationId(1), BILLED))
      )
      result.map(_.map(_.metadata.tags)) shouldBe Right(
        List(
          Set(
            AppointmentScopes.Appointment(appointment).toTag,
            AppointmentScopes.Prestation(prestationId(1)).toTag
          )
        )
      )
    }
  }

  it should "follow each prestation of an appointment on its own" in {
    val test =
      for
        world <- withPrestation
        _ <- addPrestation(world, prestationId(2)).flatMap(IO.fromEither)
        _ <- setStatus(world, prestationId(1), BILLED).flatMap(IO.fromEither)
        _ <- setStatus(world, prestationId(1), PAID).flatMap(IO.fromEither)
        // the second one is still at its first status
        second <- setStatus(world, prestationId(2), BILLED)
      yield second
    test.asserting(_.isRight shouldBe true)
  }

  it should "refuse an unknown appointment or prestation" in {
    val test =
      for
        world <- withPrestation
        unknownAppointment <- setStatus(world, prestationId(1), BILLED, of = appointmentId(9))
        unknownPrestation <- setStatus(world, prestationId(2), BILLED)
      yield (unknownAppointment, unknownPrestation)
    test.asserting(
      _ shouldBe (
        Left(APPOINTMENT_NOT_FOUND(appointmentId(9))),
        Left(PRESTATION_NOT_FOUND(appointment, prestationId(2)))
      )
    )
  }

  // Deleting

  "DeletePrestation" should "remove a prestation whatever its status" in {
    val test =
      for
        world <- withPrestation
        _ <- setStatus(world, prestationId(1), BILLED).flatMap(IO.fromEither)
        _ <- setStatus(world, prestationId(1), PAID).flatMap(IO.fromEither)
        result <- remove(world, prestationId(1))
      yield result
    test.asserting(
      _.map(_.map(_.payload)) shouldBe Right(List(PrestationDeleted(appointment, prestationId(1))))
    )
  }

  it should "leave the other prestations of the appointment alone" in {
    val test =
      for
        world <- withPrestation
        _ <- addPrestation(world, prestationId(2)).flatMap(IO.fromEither)
        _ <- remove(world, prestationId(1)).flatMap(IO.fromEither)
        gone <- setStatus(world, prestationId(1), BILLED)
        kept <- setStatus(world, prestationId(2), BILLED)
      yield (gone, kept.isRight)
    test.asserting(_ shouldBe (Left(PRESTATION_NOT_FOUND(appointment, prestationId(1))), true))
  }

  it should "refuse an unknown appointment, an unknown prestation, and one deleted already" in {
    val test =
      for
        world <- withPrestation
        unknownAppointment <- remove(world, prestationId(1), of = appointmentId(9))
        unknownPrestation <- remove(world, prestationId(2))
        _ <- remove(world, prestationId(1))
        twice <- remove(world, prestationId(1))
      yield (unknownAppointment, unknownPrestation, twice)
    test.asserting(
      _ shouldBe (
        Left(APPOINTMENT_NOT_FOUND(appointmentId(9))),
        Left(PRESTATION_NOT_FOUND(appointment, prestationId(2))),
        Left(PRESTATION_NOT_FOUND(appointment, prestationId(1)))
      )
    )
  }
