package prototype.server.core.company.domain

import java.util.UUID

import prototype.server.core.PrototypeEvent

import persistent4s.EventTypeName
import persistent4s.circe.FutureEventVersion

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How `CompanyCreated` is written to and read from the event store. Saved
  * events are kept forever, so the JSON below is a contract: if one of these
  * tests fails after a rename, old events would no longer be readable.
  */
class CompanyEventSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val codec = PrototypeEvent.eventCodec
  private val eventType = EventTypeName.of[CompanyCreated]

  private val id = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")
  private val event = CompanyCreated(
    id,
    "Example Clinic",
    "Example Street",
    "1",
    "Exampleville",
    "VD"
  )

  // The payload as it was saved: do not change it to make a test pass
  private val saved = Json.obj(
    "id" -> Json.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e"),
    "name" -> Json.fromString("Example Clinic"),
    "addressStreet" -> Json.fromString("Example Street"),
    "addressNumber" -> Json.fromString("1"),
    "addressCity" -> Json.fromString("Exampleville"),
    "addressState" -> Json.fromString("VD")
  )

  "The event codec" should "name the event after its class and give it version 1" in {
    codec.eventType(event) shouldBe eventType
    codec.eventVersion(event) shouldBe 1
  }

  it should "write every field under its own name" in {
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

  it should "reject a payload with a missing field" in {
    val withoutName = saved.mapObject(_.remove("name"))

    codec.decode(eventType, 1, withoutName.noSpaces).isLeft shouldBe true
  }

  it should "reject a payload whose id is not a UUID" in {
    val badId = saved.mapObject(_.add("id", Json.fromString("not-a-uuid")))

    codec.decode(eventType, 1, badId.noSpaces).isLeft shouldBe true
  }

  it should "reject an event saved with a newer version than it knows" in {
    codec.decode(eventType, 2, saved.noSpaces).left.value shouldBe
      a[FutureEventVersion]
  }
