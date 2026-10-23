package prototype.server.core.appointment.persistence

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.PrestationStatus
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.core.appointment.domain.*
import prototype.server.support.EventEnvelopes

import persistent4s.{
  EventEnvelope,
  EventTypeName,
  ProjectionStateAlreadyExists,
  ProjectionStateNotFound
}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the appointment projection does with each event, without a database.
  * The repository is built on a pool that fails when used, so a handler that
  * reached for the database would make the test fail.
  */
class AppointmentProjectionSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val repository = AppointmentRepository.make(
    Resource.eval(
      IO.raiseError(new AssertionError("Projection handlers must not access the database"))
    )
  )
  private val projection = AppointmentProjection(repository)

  private val id = appointmentId(1)

  private def envelope[E <: PrototypeEvent](event: E, eventType: EventTypeName): EventEnvelope[PrototypeEvent] =
    EventEnvelopes.of(event, Set(AppointmentScopes.Appointment(id).toTag), eventType)

  private val state = AppointmentState(
    id,
    List("abc"),
    Nil,
    "INDIVIDUAL",
    millis(10),
    millis(11),
    prestations = List(
      PrestationRow(prestationId(1), "Consultation", 120f, 1, PrestationStatus.TO_BILL),
      PrestationRow(prestationId(2), "Report", 80f, 2, PrestationStatus.TO_BILL)
    )
  )

  "AppointmentProjection" should "create the appointment row, with no prestations, from the creation" in {
    projection
      .handle(
        None,
        envelope(
          AppointmentCreated(id, List("abc", "xyz"), List(prestationId(9)), "COUPLE", millis(10), millis(11)),
          EventTypeName.of[AppointmentCreated]
        )
      )
      .asserting(
        _ shouldBe Some(
          AppointmentState(id, List("abc", "xyz"), List(prestationId(9)), "COUPLE", millis(10), millis(11), Nil)
        )
      )
  }

  it should "replace the type, the people and the time on an update, and keep the prestations" in {
    projection
      .handle(
        Some(state),
        envelope(
          AppointmentUpdated(id, List("xyz"), List(prestationId(9)), "FAMILY", millis(14), millis(15)),
          EventTypeName.of[AppointmentUpdated]
        )
      )
      .asserting(
        _ shouldBe Some(
          state.copy(
            therapistIds = List("xyz"),
            patientIds = List(prestationId(9)),
            apptType = "FAMILY",
            startTime = millis(14),
            endTime = millis(15)
          )
        )
      )
  }

  it should "remove the row on a deletion" in {
    projection
      .handle(Some(state), envelope(AppointmentDeleted(id), EventTypeName.of[AppointmentDeleted]))
      .asserting(_ shouldBe None)
  }

  it should "add a new prestation at the end, at its first status" in {
    projection
      .handle(
        Some(state),
        envelope(
          PrestationCreated(id, prestationId(3), "Follow-up", 60f, 1),
          EventTypeName.of[PrestationCreated]
        )
      )
      .asserting(
        _.map(_.prestations) shouldBe Some(
          state.prestations :+ PrestationRow(prestationId(3), "Follow-up", 60f, 1, PrestationStatus.TO_BILL)
        )
      )
  }

  it should "change the status of that prestation only" in {
    projection
      .handle(
        Some(state),
        envelope(
          PrestationStatusUpdated(id, prestationId(2), PrestationStatus.BILLED),
          EventTypeName.of[PrestationStatusUpdated]
        )
      )
      .asserting(
        _.map(_.prestations.map(_.status)) shouldBe Some(
          List(PrestationStatus.TO_BILL, PrestationStatus.BILLED)
        )
      )
  }

  it should "remove that prestation only" in {
    projection
      .handle(
        Some(state),
        envelope(PrestationDeleted(id, prestationId(1)), EventTypeName.of[PrestationDeleted])
      )
      .asserting(_.map(_.prestations.map(_.prestationId)) shouldBe Some(List(prestationId(2))))
  }

  it should "refuse a second creation for a row that exists" in {
    projection
      .handle(
        Some(state),
        envelope(
          AppointmentCreated(id, List("abc"), Nil, "INDIVIDUAL", millis(10), millis(11)),
          EventTypeName.of[AppointmentCreated]
        )
      )
      .attempt
      .asserting(_ shouldBe Left(ProjectionStateAlreadyExists(EventTypeName.of[AppointmentCreated])))
  }

  it should "refuse a change to a row that does not exist" in {
    projection
      .handle(
        None,
        envelope(
          PrestationCreated(id, prestationId(3), "Follow-up", 60f, 1),
          EventTypeName.of[PrestationCreated]
        )
      )
      .attempt
      .asserting(_ shouldBe Left(ProjectionStateNotFound(EventTypeName.of[PrestationCreated])))
  }
