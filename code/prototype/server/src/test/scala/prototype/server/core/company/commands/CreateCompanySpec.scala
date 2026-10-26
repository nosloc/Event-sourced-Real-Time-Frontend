package prototype.server.core.company.commands

import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.domain.{CompanyCreated, CompanyScopes}
import prototype.server.core.company.domain.CompanyError.*
import prototype.server.support.InMemoryEventStore

import persistent4s.CommandRuntime

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `CreateCompany` handler on an in-memory event store: what it saves, how
  * it tags it, and that every rejection leaves the store untouched. No database
  * is involved, so these tests are fast.
  */
class CreateCompanySpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val id = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")
  private val address = Address("Example Street", "1", "Exampleville", "VD")
  private val create = CreateCompany(id, "Example Clinic", address)

  private val created = CompanyCreated(
    id,
    "Example Clinic",
    "Example Street",
    "1",
    "Exampleville",
    "VD"
  )

  private final class Setup(
      val store: InMemoryEventStore[PrototypeEvent],
      val runtime: CommandRuntime[IO, PrototypeEvent]
  )

  // A fresh store and runtime per test
  private val setup: IO[Setup] =
    InMemoryEventStore
      .make[PrototypeEvent]
      .map(store => Setup(store, CommandRuntime.eventStoreOnly(store)))

  private val unique = CompanyScopes.CompanyUnique(()).toTag

  "CreateCompany" should "save a CompanyCreated event tagged with the company and the shared unique tag" in {
    val test =
      for
        s <- setup
        result <- s.runtime.execute(CreateCompany.Handler, create)
        saved <- s.store.all
      yield (result, saved)

    test.asserting { case (result, saved) =>
      result.map(_.map(_.payload)) shouldBe Right(List(created))
      result.map(_.map(_.metadata.tags)) shouldBe
        Right(List(Set(CompanyScopes.Company(id).toTag, unique)))
      saved.toList shouldBe result.toOption.toList.flatten
    }
  }

  it should "keep the history of each company under its own tag" in {
    val otherId = UUID.fromString("070874c8-5296-452e-8c91-cfaf54385932")
    val test =
      for
        s <- setup
        _ <- s.runtime.execute(CreateCompany.Handler, create)
        _ <- s.runtime.execute(
          CreateCompany.Handler,
          create.copy(id = otherId, name = "Other Clinic")
        )
        saved <- s.store.all
      yield saved

    test.asserting { saved =>
      saved.map(_.metadata.tags) shouldBe Vector(
        Set(CompanyScopes.Company(id).toTag, unique),
        Set(CompanyScopes.Company(otherId).toTag, unique)
      )
    }
  }

  private val rejected = List(
    ("an empty name", create.copy(name = ""), COMPANY_NAME_EMPTY(id)),
    ("a name of spaces", create.copy(name = " \t"), COMPANY_NAME_EMPTY(id)),
    (
      "an empty street",
      create.copy(address = address.copy(street = "")),
      COMPANY_ADDRESS_INVALID(id)
    ),
    (
      "an empty number",
      create.copy(address = address.copy(number = "")),
      COMPANY_ADDRESS_INVALID(id)
    ),
    (
      "an empty city",
      create.copy(address = address.copy(city = "")),
      COMPANY_ADDRESS_INVALID(id)
    ),
    (
      "an empty state",
      create.copy(address = address.copy(state = " ")),
      COMPANY_ADDRESS_INVALID(id)
    )
  )

  rejected.foreach { (description, command, error) =>
    "CreateCompany" should s"reject $description without saving an event" in {
      val test =
        for
          s <- setup
          result <- s.runtime.execute(CreateCompany.Handler, command)
          saved <- s.store.all
        yield (result, saved)

      test.asserting { case (result, saved) =>
        result shouldBe Left(error)
        saved shouldBe empty
      }
    }
  }

  it should "reject a second creation with the same id and keep a single event" in {
    val test =
      for
        s <- setup
        first <- s.runtime.execute(CreateCompany.Handler, create)
        second <- s.runtime.execute(CreateCompany.Handler, create)
        saved <- s.store.all
      yield (first, second, saved)

    test.asserting { case (first, second, saved) =>
      first.isRight shouldBe true
      second shouldBe Left(COMPANY_ALREADY_EXISTS(id))
      saved.toList shouldBe first.toOption.toList.flatten
    }
  }

  it should "accept only one of two concurrent creations of the same id" in {
    val test =
      for
        s <- setup
        results <- (
          s.runtime.execute(CreateCompany.Handler, create),
          s.runtime.execute(CreateCompany.Handler, create)
        ).parTupled
        saved <- s.store.all
      yield (results, saved)

    test.asserting { case ((first, second), saved) =>
      List(first, second).count(_.isRight) shouldBe 1
      List(first, second).collect { case Left(error) => error } shouldBe
        List(COMPANY_ALREADY_EXISTS(id))
      saved should have size 1
    }
  }
