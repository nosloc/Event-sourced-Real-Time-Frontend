package prototype.server.core.therapist.commands

import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

import prototype.api.{Address, Role}
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.commands.CreateCompany
import prototype.server.core.therapist.domain.{TherapistCreated, TherapistScopes}
import prototype.server.core.therapist.domain.TherapistError.*
import prototype.server.support.InMemoryEventStore

import persistent4s.CommandRuntime

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `CreateTherapist` handler on an in-memory event store: what it saves, how
  * it tags it, and that every rejection leaves the store as it was. The handler
  * reads the company's own events to know that the company exists.
  */
class CreateTherapistSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val company = UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e")
  private val create =
    CreateTherapist("abc", "Ann", "Lee", company, Role.PHYSICIAN)

  private final class Setup(
      val store: InMemoryEventStore[PrototypeEvent],
      val runtime: CommandRuntime[IO, PrototypeEvent]
  ):
    def run(command: CreateTherapist) =
      runtime.execute(CreateTherapist.Handler, command)

    // The therapist's events only, without the company's creation
    def therapistEvents =
      store.all.map(_.filter(_.payload.isInstanceOf[TherapistCreated]))

  // A fresh store per test, with or without the company
  private def setup(withCompany: Boolean = true): IO[Setup] =
    for
      store <- InMemoryEventStore.make[PrototypeEvent]
      runtime = CommandRuntime.eventStoreOnly[IO, PrototypeEvent](store)
      _ <- IO.whenA(withCompany)(
        runtime
          .execute(
            CreateCompany.Handler,
            CreateCompany(
              company,
              "Example Clinic",
              Address("Example Street", "1", "Exampleville", "VD")
            )
          )
          .flatMap(IO.fromEither)
          .void
      )
    yield Setup(store, runtime)

  "CreateTherapist" should "save a TherapistCreated event tagged with the therapist only" in {
    val test =
      for
        s <- setup()
        result <- s.run(create)
      yield result

    test.asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(
        List(TherapistCreated("abc", "Ann", "Lee", company, Role.PHYSICIAN))
      )
      // Not the company: a company's stream does not list its therapists
      result.map(_.map(_.metadata.tags)) shouldBe
        Right(List(Set(TherapistScopes.Therapist("abc").toTag)))
    }
  }

  it should "accept several therapists in the same company" in {
    val test =
      for
        s <- setup()
        first <- s.run(create)
        second <- s.run(create.copy(therapistId = "xyz"))
        saved <- s.therapistEvents
      yield (first, second, saved)

    test.asserting { case (first, second, saved) =>
      first.isRight shouldBe true
      second.isRight shouldBe true
      saved.map(_.metadata.tags) shouldBe Vector(
        Set(TherapistScopes.Therapist("abc").toTag),
        Set(TherapistScopes.Therapist("xyz").toTag)
      )
    }
  }

  it should "refuse a company that does not exist, without saving an event" in {
    val test =
      for
        s <- setup(withCompany = false)
        result <- s.run(create)
        saved <- s.store.all
      yield (result, saved)

    test.asserting { case (result, saved) =>
      result shouldBe Left(THERAPIST_COMPANY_NOT_FOUND("abc"))
      saved shouldBe empty
    }
  }

  private val badNames = List(
    ("an empty first name", create.copy(firstName = "")),
    ("an empty last name", create.copy(lastName = "")),
    ("a first name of spaces", create.copy(firstName = " \t"))
  )

  badNames.foreach { (description, command) =>
    it should s"refuse $description without saving an event" in {
      val test =
        for
          s <- setup()
          result <- s.run(command)
          saved <- s.therapistEvents
        yield (result, saved)

      test.asserting { case (result, saved) =>
        result shouldBe Left(THERAPIST_NAME_EMPTY("abc"))
        saved shouldBe empty
      }
    }
  }

  it should "refuse a taken trigram, even in another company, and keep a single event" in {
    val other = UUID.fromString("070874c8-5296-452e-8c91-cfaf54385932")
    val test =
      for
        s <- setup()
        first <- s.run(create)
        second <- s.run(create.copy(companyId = other, firstName = "Bob"))
        saved <- s.therapistEvents
      yield (first, second, saved)

    test.asserting { case (first, second, saved) =>
      first.isRight shouldBe true
      // The trigram is checked first: the other company does not even exist
      second shouldBe Left(THERAPIST_ALREADY_EXISTS("abc"))
      saved.toList shouldBe first.toOption.toList.flatten
    }
  }

  it should "accept only one of two concurrent creations of the same trigram" in {
    val test =
      for
        s <- setup()
        results <- (s.run(create), s.run(create.copy(firstName = "Bob"))).parTupled
        saved <- s.therapistEvents
      yield (results, saved)

    test.asserting { case ((first, second), saved) =>
      List(first, second).count(_.isRight) shouldBe 1
      List(first, second).collect { case Left(error) => error } shouldBe
        List(THERAPIST_ALREADY_EXISTS("abc"))
      saved should have size 1
    }
  }
