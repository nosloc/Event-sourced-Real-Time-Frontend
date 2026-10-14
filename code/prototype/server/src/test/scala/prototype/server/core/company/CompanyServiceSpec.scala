package prototype.server.core.company

import java.util.UUID
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.eventStore.InMemoryEventStore

/** The company service end to end, with the real decider, viewer and store.
  * `CompanyAlreadyExists` has no test: the service generates a fresh random id
  * for every company, so a duplicate cannot happen through the API.
  */
class CompanyServiceSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val address = Address("Example Street", "1", "Exampleville", "VD")
  private val otherAddress = Address("Sample Avenue", "2", "Sampletown", "GE")

  // A fresh service (and store) per test
  private val newService: IO[CompanyService[IO]] =
    InMemoryEventStore.create.flatMap(CompanyServiceImpl(_))

  "createCompany" should "return the company with the given name and address" in {
    val test = newService.flatMap(_.createCompany("Example Clinic", address))

    test.asserting { created =>
      created.company.name shouldBe "Example Clinic"
      created.company.address shouldBe address
    }
  }

  it should "give every company its own id" in {
    val test =
      for
        service <- newService
        first <- service.createCompany("Example Clinic", address)
        second <- service.createCompany("Demo Health Center", otherAddress)
      yield (first.company.companyId, second.company.companyId)

    test.asserting { case (firstId, secondId) =>
      firstId should not be secondId
    }
  }

  it should "make the company available through getCompany and listCompanies" in {
    val test =
      for
        service <- newService
        created <- service.createCompany("Example Clinic", address)
        fetched <- service.getCompany(created.company.companyId)
        listed <- service.listCompanies()
      yield (created, fetched, listed)

    test.asserting { case (created, fetched, listed) =>
      fetched.company shouldBe created.company
      listed.companies shouldBe List(created.company)
    }
  }

  "listCompanies" should "be empty before any company is created" in {
    newService
      .flatMap(_.listCompanies())
      .asserting(_.companies shouldBe Nil)
  }

  it should "list every created company" in {
    val test =
      for
        service <- newService
        first <- service.createCompany("Example Clinic", address)
        second <- service.createCompany("Demo Health Center", otherAddress)
        listed <- service.listCompanies()
      yield (first.company, second.company, listed.companies)

    test.asserting { case (first, second, listed) =>
      // A map has no order, so compare as a set
      listed should contain theSameElementsAs List(first, second)
    }
  }

  "getCompany" should "fail with CompanyNotFound for an unknown id" in {
    val unknown = CompanyId(UUID.randomUUID())
    val test = newService.flatMap(_.getCompany(unknown))

    test.attempt.asserting(_ shouldBe Left(CompanyNotFound()))
  }
