package prototype.server.core.patient.domain

import java.time.LocalDate
import java.util.UUID

import prototype.server.core.PrototypeEvent

import persistent4s.EventTypeName
import persistent4s.circe.FutureEventVersion

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How `PatientCreated` is written to and read from the event store. Saved
  * events are kept forever, so the JSON below is a contract: if one of these
  * tests fails after a rename, old events would no longer be readable.
  */
class PatientEventSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val codec = PrototypeEvent.eventCodec
  private val eventType = EventTypeName.of[PatientCreated]

  private val id = UUID.fromString("070874c8-5296-452e-8c91-cfaf54385932")
  private val event =
    PatientCreated(id, "Camille", "Rochat", LocalDate.of(1987, 4, 12))

  // The payload as it was saved: do not change it to make a test pass
  private val saved = Json.obj(
    "id" -> Json.fromString("070874c8-5296-452e-8c91-cfaf54385932"),
    "firstName" -> Json.fromString("Camille"),
    "lastName" -> Json.fromString("Rochat"),
    "dateOfBirth" -> Json.fromString("1987-04-12")
  )

  "The event codec" should "name the event after its class and give it version 1" in {
    codec.eventType(event) shouldBe eventType
    codec.eventVersion(event) shouldBe 1
  }

  it should "write every field under its own name, the date as year-month-day" in {
    parse(codec.encode(event).value).value shouldBe saved
  }

  it should "read back what it wrote" in {
    val encoded = codec.encodeWithSchema(event).value

    codec.decode(encoded.eventType, encoded.version, encoded.payload) shouldBe
      Right(event)
  }

  it should "read an event that was saved earlier" in {
    codec.decode(eventType, 1, saved.noSpaces) shouldBe Right(event)
  }

  it should "reject a date of birth that is not a date" in {
    val badDate =
      saved.mapObject(_.add("dateOfBirth", Json.fromString("12.04.1987")))

    codec.decode(eventType, 1, badDate.noSpaces).isLeft shouldBe true
  }

  it should "reject a payload with a missing field" in {
    val withoutDate = saved.mapObject(_.remove("dateOfBirth"))

    codec.decode(eventType, 1, withoutDate.noSpaces).isLeft shouldBe true
  }

  it should "reject an event saved with a newer version than it knows" in {
    codec.decode(eventType, 2, saved.noSpaces).left.value shouldBe
      a[FutureEventVersion]
  }
