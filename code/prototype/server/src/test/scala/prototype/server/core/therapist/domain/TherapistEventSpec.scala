package prototype.server.core.therapist.domain

import java.util.UUID

import prototype.api.Role
import prototype.server.core.PrototypeEvent

import persistent4s.EventTypeName
import persistent4s.circe.FutureEventVersion

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How `TherapistCreated` is written to and read from the event store. Saved
  * events are kept forever, so the JSON below is a contract: if one of these
  * tests fails after a rename, old events would no longer be readable.
  */
class TherapistEventSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val codec = PrototypeEvent.eventCodec
  private val eventType = EventTypeName.of[TherapistCreated]

  private val company = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")
  private val event =
    TherapistCreated("abc", "Ann", "Lee", company, Role.PHYSICIAN)

  // The payload as it was saved: do not change it to make a test pass
  private val saved = Json.obj(
    "id" -> Json.fromString("abc"),
    "firstName" -> Json.fromString("Ann"),
    "lastName" -> Json.fromString("Lee"),
    "companyId" -> Json.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e"),
    "role" -> Json.fromString("PHYSICIAN")
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

  // The role is stored as its name, so renaming a role breaks old events
  Role.values.foreach { role =>
    it should s"save the role $role as its name and read it back" in {
      val withRole = event.copy(role = role)
      val json = parse(codec.encode(withRole).value).value

      json.hcursor.get[String]("role") shouldBe Right(role.stringValue)
      codec.decode(eventType, 1, json.noSpaces) shouldBe Right(withRole)
    }
  }

  it should "reject a role it does not know" in {
    val unknown = saved.mapObject(_.add("role", Json.fromString("SURGEON")))

    codec.decode(eventType, 1, unknown.noSpaces).isLeft shouldBe true
  }

  it should "reject a payload with a missing field" in {
    val withoutRole = saved.mapObject(_.remove("role"))

    codec.decode(eventType, 1, withoutRole.noSpaces).isLeft shouldBe true
  }

  it should "reject an event saved with a newer version than it knows" in {
    codec.decode(eventType, 2, saved.noSpaces).left.value shouldBe
      a[FutureEventVersion]
  }
