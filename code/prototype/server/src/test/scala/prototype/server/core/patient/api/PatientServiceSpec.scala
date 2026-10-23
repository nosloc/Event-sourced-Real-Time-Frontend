package prototype.server.core.patient.api

import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.App
import prototype.server.support.{PostgresSuite, TestApp}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import smithy4s.time.LocalDate

/** The patient service on a real Postgres in Docker: command, event store,
  * projection and read model together. A creation does not wait for the
  * projection, so reads are repeated until it has caught up. Docker must be
  * running.
  */
class PatientServiceSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val born = LocalDate(1987, 4, 12)

  private def withApp[A](test: App.Services => IO[A]): IO[A] =
    freshDatabase.use(postgres => TestApp.make(postgres).use(test))

  "The patient service" should "give a new patient an id, readable once the projection has caught up" in {
    withApp { app =>
      for
        id <- app.patients.createPatient("Camille", "Rochat", born).map(_.patientId)
        read <- TestApp.eventually(
          app.patients.getPatient(id).map(Some(_)).handleError(_ => None)
        )(_.isDefined)
      yield (id, read.map(_.patient))
    }.asserting { case (id, read) =>
      read shouldBe Some(Patient(id, "Camille", "Rochat", born))
    }
  }

  it should "turn the refusals into the API's errors" in {
    withApp { app =>
      for
        _ <- app.patients.createPatient("Camille", "Rochat", born)
        // The duplicate check reads the events: no need to wait for the projection
        again <- app.patients.createPatient("Camille", "Rochat", born).attempt
        noName <- app.patients.createPatient("", "Favre", born).attempt
      yield (again, noName)
    }.asserting { case (again, noName) =>
      again shouldBe Left(PatientAlreadyExists())
      noName shouldBe Left(InvalidPatient(List("firstName")))
    }
  }

  it should "list the patients sorted by last name, then first name" in {
    withApp { app =>
      for
        _ <- app.patients.createPatient("Noah", "Rochat", born)
        _ <- app.patients.createPatient("Lea", "Favre", born)
        _ <- app.patients.createPatient("Camille", "Rochat", born)
        listed <- TestApp.eventually(
          app.patients.listPatients().map(_.patients)
        )(_.size == 3)
      yield listed
    }.asserting { listed =>
      listed.map(p => (p.lastName, p.firstName)) shouldBe List(
        "Favre" -> "Lea",
        "Rochat" -> "Camille",
        "Rochat" -> "Noah"
      )
    }
  }

  it should "report an unknown patient as not found" in {
    val unknown = PatientId(UUID.fromString("00000000-0000-0000-0000-000000000099"))
    withApp(_.patients.getPatient(unknown).attempt)
      .asserting(_ shouldBe Left(PatientNotFound(unknown)))
  }
