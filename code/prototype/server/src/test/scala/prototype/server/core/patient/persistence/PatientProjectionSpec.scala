package prototype.server.core.patient.persistence

import java.time.LocalDate
import java.util.UUID

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.server.core.PrototypeEvent
import prototype.server.core.patient.domain.{PatientCreated, PatientScopes}
import prototype.server.support.EventEnvelopes

import persistent4s.{EventEnvelope, EventTypeName, ProjectionStateAlreadyExists}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the projection does with a `PatientCreated` event, without a database.
  * The repository is built on a pool that fails when used, so a handler that
  * reached for the database would make the test fail.
  */
class PatientProjectionSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val repository = PatientRepository.make(
    Resource.eval(
      IO.raiseError(
        new AssertionError("Projection handlers must not access the database")
      )
    )
  )
  private val projection = PatientProjection(repository)

  private val id = UUID.fromString("070874c8-5296-452e-8c91-cfaf54385932")
  private val born = LocalDate.of(1987, 4, 12)

  private val envelope: EventEnvelope[PrototypeEvent] =
    EventEnvelopes.of(
      PatientCreated(id, "Camille", "Rochat", born),
      Set(PatientScopes.Patient(id).toTag, PatientScopes.PatientUnique(()).toTag),
      EventTypeName.of[PatientCreated]
    )

  "PatientProjection" should "create the patient row from the creation event" in {
    projection.handle(None, envelope).asserting { row =>
      row shouldBe Some(PatientState(id, "Camille", "Rochat", born))
    }
  }

  it should "refuse a second creation for a patient that already has a row" in {
    val existing = PatientState(id, "Old", "Old", born)
    projection
      .handle(Some(existing), envelope)
      .attempt
      .asserting { result =>
        result shouldBe Left(
          ProjectionStateAlreadyExists(EventTypeName.of[PatientCreated])
        )
      }
  }
