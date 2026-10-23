package prototype.server.core.therapist.api

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.App
import prototype.server.support.{PostgresSuite, TestApp}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The therapist service on a real Postgres in Docker: command, event store,
  * projection and read model together. A creation does not wait for the
  * projection, so reads are repeated until it has caught up. Docker must be
  * running.
  */
class TherapistServiceSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val abc = TherapistId("abc")
  private val xyz = TherapistId("xyz")

  // The app on a fresh database, with one company
  private def withApp[A](test: (App.Services, CompanyId) => IO[A]): IO[A] =
    freshDatabase.use(postgres =>
      TestApp.make(postgres).use(app =>
        app.companies
          .createCompany("Example Clinic", Address("Example Street", "1", "Exampleville", "VD"))
          .flatMap(created => test(app, created.companyId))
      )
    )

  "The therapist service" should "answer a creation with the therapist, readable once the projection has caught up" in {
    withApp { (app, company) =>
      for
        created <- app.therapists.createTherapist("Ann", "Lee", abc, company, Role.PHYSICIAN)
        read <- TestApp.eventually(
          app.therapists.getTherapist(abc).map(Some(_)).handleError(_ => None)
        )(_.isDefined)
      yield (created.therapist, read.map(_.therapist))
    }.asserting { case (created, read) =>
      created shouldBe Therapist("Ann", "Lee", abc, created.companyId, Role.PHYSICIAN)
      read shouldBe Some(created)
    }
  }

  it should "turn the refusals into the API's errors" in {
    withApp { (app, company) =>
      for
        _ <- app.therapists.createTherapist("Ann", "Lee", abc, company, Role.PHYSICIAN)
        taken <- app.therapists
          .createTherapist("Bob", "Ray", abc, company, Role.PSYCHOLOGIST)
          .attempt
        noCompany <- app.therapists
          .createTherapist("Bob", "Ray", xyz, CompanyId(java.util.UUID.randomUUID()), Role.PSYCHOLOGIST)
          .attempt
        noName <- app.therapists
          .createTherapist(" ", "Ray", xyz, company, Role.PSYCHOLOGIST)
          .attempt
      yield (taken, noCompany, noName)
    }.asserting { case (taken, noCompany, noName) =>
      taken shouldBe Left(TherapistAlreadyExists())
      noCompany shouldBe Left(CompanyNotFound())
      noName shouldBe Left(InvalidTherapist())
    }
  }

  it should "list the therapists sorted by id, each with its role" in {
    withApp { (app, company) =>
      for
        _ <- app.therapists.createTherapist("Bob", "Ray", xyz, company, Role.GENERAL_PRACTITIONER)
        _ <- app.therapists.createTherapist("Ann", "Lee", abc, company, Role.NEUROPSYCHOLOGIST)
        listed <- TestApp.eventually(
          app.therapists.listTherapists().map(_.therapists)
        )(_.size == 2)
      yield listed
    }.asserting { listed =>
      listed.map(t => (t.therapistId, t.role)) shouldBe List(
        abc -> Role.NEUROPSYCHOLOGIST,
        xyz -> Role.GENERAL_PRACTITIONER
      )
    }
  }

  it should "report an unknown therapist as not found" in {
    withApp { (app, _) =>
      app.therapists.getTherapist(abc).attempt
    }.asserting(_ shouldBe Left(TherapistNotFound(abc)))
  }
