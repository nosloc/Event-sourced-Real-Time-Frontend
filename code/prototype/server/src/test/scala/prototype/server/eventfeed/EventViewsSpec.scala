package prototype.server.eventfeed

import java.time.Instant
import java.util.UUID

import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.AppointmentCreated
import prototype.server.support.EventEnvelopes

import persistent4s.{EventTypeName, Tag}

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import smithy4s.time.Timestamp

/** A stored event as the browser sees it: the metadata the store keeps with
  * it, and its content exactly as the store encodes it.
  */
class EventViewsSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val codec = PrototypeEvent.eventCodec

  private val appointmentId = UUID.fromString("5b1e3c2a-8f4d-4c1e-9a7b-2d6f0e8c9a11")
  private val patientId = UUID.fromString("0c9d8e7f-6a5b-4c3d-8e2f-1a0b9c8d7e6f")
  private val eventId = UUID.fromString("3f1ce2a4-7b8c-4d9e-a0f1-e2d3c4b5a6e9")
  private val recordedAt = Instant.parse("2026-10-26T14:32:07Z")

  private val event: PrototypeEvent = AppointmentCreated(
    appointmentId,
    List("tst"),
    List(patientId),
    "INDIVIDUAL",
    36000000L,
    39600000L
  )

  // Tags given out of order, to see that they come back sorted
  private val envelope =
    val built = EventEnvelopes.of(
      event,
      Set(
        Tag("therapist", "tst"),
        Tag("patient", patientId),
        Tag("appointment", appointmentId)
      ),
      EventTypeName.of[AppointmentCreated]
    )
    built.copy(metadata =
      built.metadata.copy(globalPosition = 42L, id = eventId, timestamp = recordedAt)
    )

  "EventViews.from" should "copy the metadata of the stored event" in {
    val view = EventViews.from(envelope, codec).value
    view.position shouldBe 42L
    view.eventId shouldBe eventId.toString
    view.eventType shouldBe "AppointmentCreated"
    view.recordedAt shouldBe Timestamp.fromInstant(recordedAt)
  }

  it should "give the tags as category:id, sorted" in {
    EventViews.from(envelope, codec).value.tags shouldBe List(
      s"appointment:$appointmentId",
      s"patient:$patientId",
      "therapist:tst"
    )
  }

  it should "carry the content exactly as the store encodes it" in {
    EventViews.from(envelope, codec).value.payload shouldBe
      codec.encode(event).value
  }
end EventViewsSpec
