package prototype.server.core.therapist.persistence

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.Role
import prototype.server.core.PrototypeEvent
import prototype.server.core.therapist.domain.{TherapistCreated, TherapistScopes}
import prototype.server.support.EventEnvelopes

import persistent4s.{EventEnvelope, EventTypeName, ProjectionStateAlreadyExists}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the projection does with a `TherapistCreated` event, without a
  * database. The repository is built on a pool that fails when used, so a
  * handler that reached for the database would make the test fail.
  */
class TherapistProjectionSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val repository = TherapistRepository.make(
    Resource.eval(
      IO.raiseError(
        new AssertionError("Projection handlers must not access the database")
      )
    )
  )
  private val projection = TherapistProjection(repository)

  private val company = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")

  private val envelope: EventEnvelope[PrototypeEvent] =
    EventEnvelopes.of(
      TherapistCreated("abc", "Ann", "Lee", company, Role.NEUROPSYCHOLOGIST),
      Set(TherapistScopes.Therapist("abc").toTag),
      EventTypeName.of[TherapistCreated]
    )

  "TherapistProjection" should "create the therapist row, with the role's name, from the creation event" in {
    projection.handle(None, envelope).asserting { row =>
      row shouldBe Some(
        TherapistState("abc", "Ann", "Lee", company, "NEUROPSYCHOLOGIST")
      )
    }
  }

  it should "refuse a second creation for a therapist that already has a row" in {
    val existing = TherapistState("abc", "Old", "Old", company, "PHYSICIAN")
    projection
      .handle(Some(existing), envelope)
      .attempt
      .asserting { result =>
        result shouldBe Left(
          ProjectionStateAlreadyExists(EventTypeName.of[TherapistCreated])
        )
      }
  }
