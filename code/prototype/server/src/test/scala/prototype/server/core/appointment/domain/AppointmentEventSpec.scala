package prototype.server.core.appointment.domain

import java.util.UUID

import prototype.api.PrestationStatus
import prototype.server.core.PrototypeEvent

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import persistent4s.EventTypeName
import persistent4s.circe.FutureEventVersion

/** The JSON of the appointment events as they are saved. Saved events are kept
  * for ever, so this JSON is a contract: the `saved` payloads below must keep
  * decoding. Do not change them to make a test pass.
  */
class AppointmentEventSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val codec = PrototypeEvent.eventCodec

  private val appointment = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")
  private val patient = UUID.fromString("070874c8-5296-452e-8c91-cfaf54385932")
  private val prestation = UUID.fromString("5b7c2d41-8a3e-4f6b-9c0d-2e1f3a4b5c6d")

  private def str(value: String) = Json.fromString(value)

  private val people = Seq(
    "id" -> str(appointment.toString),
    "therapistIds" -> Json.arr(str("abc"), str("xyz")),
    "patientIds" -> Json.arr(str(patient.toString)),
    "apptType" -> str("INDIVIDUAL"),
    "startTime" -> Json.fromLong(36000000L),
    "endTime" -> Json.fromLong(39600000L)
  )

  // Each event, and the JSON it was saved as
  private val cases: List[(PrototypeEvent, EventTypeName, Json)] = List(
    (
      AppointmentCreated(appointment, List("abc", "xyz"), List(patient), "INDIVIDUAL", 36000000L, 39600000L),
      EventTypeName.of[AppointmentCreated],
      Json.obj(people*)
    ),
    (
      AppointmentUpdated(appointment, List("abc", "xyz"), List(patient), "INDIVIDUAL", 36000000L, 39600000L),
      EventTypeName.of[AppointmentUpdated],
      Json.obj(people*)
    ),
    (
      AppointmentDeleted(appointment),
      EventTypeName.of[AppointmentDeleted],
      Json.obj("id" -> str(appointment.toString))
    ),
    (
      PrestationCreated(appointment, prestation, "Consultation", 120.5f, 2),
      EventTypeName.of[PrestationCreated],
      Json.obj(
        "id" -> str(appointment.toString),
        "prestationId" -> str(prestation.toString),
        "label" -> str("Consultation"),
        "unitPrice" -> Json.fromFloatOrNull(120.5f),
        "quantity" -> Json.fromInt(2)
      )
    ),
    (
      PrestationStatusUpdated(appointment, prestation, PrestationStatus.BILLED),
      EventTypeName.of[PrestationStatusUpdated],
      Json.obj(
        "id" -> str(appointment.toString),
        "prestationId" -> str(prestation.toString),
        "status" -> str("BILLED")
      )
    ),
    (
      PrestationDeleted(appointment, prestation),
      EventTypeName.of[PrestationDeleted],
      Json.obj(
        "id" -> str(appointment.toString),
        "prestationId" -> str(prestation.toString)
      )
    )
  )

  cases.foreach { (event, eventType, saved) =>
    val name = eventType.value

    s"$name" should "be named after its class, at version 1" in {
      codec.eventType(event) shouldBe eventType
      codec.eventVersion(event) shouldBe 1
    }

    it should "write every field under its own name" in {
      parse(codec.encode(event).value).value shouldBe saved
    }

    it should "read an event that was saved earlier" in {
      codec.decode(eventType, 1, saved.noSpaces) shouldBe Right(event)
    }

    it should "refuse an event saved with a newer version than it knows" in {
      codec.decode(eventType, 2, saved.noSpaces).left.value shouldBe a[FutureEventVersion]
    }
  }

  "A prestation status" should "be saved as its name, for every status" in {
    PrestationStatus.values.foreach { status =>
      val event = PrestationStatusUpdated(appointment, prestation, status)
      parse(codec.encode(event).value).value.hcursor.get[String]("status") shouldBe
        Right(status.stringValue)
    }
  }

  it should "refuse a name it does not know" in {
    val unknown = Json.obj(
      "id" -> str(appointment.toString),
      "prestationId" -> str(prestation.toString),
      "status" -> str("REFUNDED")
    )
    codec
      .decode(EventTypeName.of[PrestationStatusUpdated], 1, unknown.noSpaces)
      .isLeft shouldBe true
  }
