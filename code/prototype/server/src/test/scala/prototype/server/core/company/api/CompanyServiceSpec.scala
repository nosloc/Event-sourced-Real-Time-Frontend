package prototype.server.core.company.api

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.support.{PostgresSuite, TestApp}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The company service on a real Postgres in Docker: command, event store,
  * projection and read model together. Docker must be running.
  */
class CompanyServiceSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val address = Address("Example Street", "1", "Exampleville", "VD")

  "The company service" should "return a created company once the projection has caught up" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          created <- app.companies.createCompany("Example Clinic", address)
          found <- TestApp.eventually(
            app.companies.listCompanies().map(_.companies)
          )(_.exists(_.companyId == created.companyId))
        yield found.find(_.companyId == created.companyId) shouldBe
          Some(Company(created.companyId, "Example Clinic", address))
      }
    }
  }

  it should "report an unknown company as not found" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        app.companies
          .getCompany(CompanyId(java.util.UUID.randomUUID()))
          .attempt
          .map(_ shouldBe Left(CompanyNotFound()))
      }
    }
  }

  it should "still have its companies after the application restarts" in {
    freshDatabase.use { postgres =>
      for
        created <- TestApp.make(postgres).use { app =>
          for
            created <- app.companies.createCompany("Example Clinic", address)
            _ <- TestApp.eventually(
              app.companies.listCompanies().map(_.companies)
            )(_.exists(_.companyId == created.companyId))
          yield created
        }
        // A second application on the same database: nothing is in memory
        found <- TestApp.make(postgres).use { app =>
          app.companies.getCompany(created.companyId)
        }
      yield found.company.name shouldBe "Example Clinic"
    }
  }
