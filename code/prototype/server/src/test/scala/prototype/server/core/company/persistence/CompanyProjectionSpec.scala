package prototype.server.core.company.persistence

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.server.core.PrototypeEvent
import prototype.server.core.company.domain.{CompanyCreated, CompanyScopes}
import prototype.server.support.EventEnvelopes

import persistent4s.{EventEnvelope, EventTypeName, ProjectionStateAlreadyExists}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the projection does with a `CompanyCreated` event, without a database.
  * The repository is built on a pool that fails when used, so a handler that
  * reached for the database would make the test fail.
  */
class CompanyProjectionSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val repository = CompanyRepository.make(
    Resource.eval(
      IO.raiseError(
        new AssertionError("Projection handlers must not access the database")
      )
    )
  )
  private val projection = CompanyProjection(repository)

  private val id = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")

  private val created = CompanyCreated(
    id,
    "Example Clinic",
    "Example Street",
    "1",
    "Exampleville",
    "VD"
  )

  private val envelope: EventEnvelope[PrototypeEvent] =
    EventEnvelopes.of(
      created,
      Set(CompanyScopes.Company(id).toTag),
      EventTypeName.of[CompanyCreated]
    )

  "CompanyProjection" should "create the company row from the creation event" in {
    projection.handle(None, envelope).asserting { row =>
      row shouldBe Some(
        CompanyState(
          id,
          "Example Clinic",
          "Example Street",
          "1",
          "Exampleville",
          "VD"
        )
      )
    }
  }

  it should "refuse a second creation for a company that already has a row" in {
    val existing = CompanyState(id, "Old", "Old", "0", "Old", "GE")
    projection
      .handle(Some(existing), envelope)
      .attempt
      .asserting { result =>
        result shouldBe Left(
          ProjectionStateAlreadyExists(EventTypeName.of[CompanyCreated])
        )
      }
  }
