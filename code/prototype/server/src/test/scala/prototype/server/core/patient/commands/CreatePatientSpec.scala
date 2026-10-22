package prototype.server.core.patient.commands

import java.time.LocalDate
import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

import prototype.server.core.PrototypeEvent
import prototype.server.core.patient.domain.{PatientCreated, PatientScopes}
import prototype.server.core.patient.domain.PatientError.*
import prototype.server.support.InMemoryEventStore

import persistent4s.CommandRuntime

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `CreatePatient` handler on an in-memory event store. Besides the id, a
  * patient is unique by name and date of birth: every creation also reads the
  * shared `patient-unique` stream, where all patients are.
  */
class CreatePatientSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val id = UUID.fromString("00000000-0000-0000-0000-0000000000c1")
  private val otherId = UUID.fromString("00000000-0000-0000-0000-0000000000c2")
  private val born = LocalDate.of(1987, 4, 12)
  private val create = CreatePatient(id, "Camille", "Rochat", born)

  private final class Setup(
      val store: InMemoryEventStore[PrototypeEvent],
      val runtime: CommandRuntime[IO, PrototypeEvent]
  ):
    def run(command: CreatePatient) =
      runtime.execute(CreatePatient.Handler, command)

  // A fresh store and runtime per test
  private val setup: IO[Setup] =
    InMemoryEventStore
      .make[PrototypeEvent]
      .map(store => Setup(store, CommandRuntime.eventStoreOnly(store)))

  "CreatePatient" should "save a PatientCreated event in the patient's stream and the shared one" in {
    setup.flatMap(_.run(create)).asserting { result =>
      result.map(_.map(_.payload)) shouldBe Right(
        List(PatientCreated(id, "Camille", "Rochat", born))
      )
      result.map(_.map(_.metadata.tags)) shouldBe Right(
        List(
          Set(
            PatientScopes.Patient(id).toTag,
            PatientScopes.PatientUnique(()).toTag
          )
        )
      )
    }
  }

  it should "accept a namesake born on another day" in {
    val test =
      for
        s <- setup
        first <- s.run(create)
        second <- s.run(create.copy(id = otherId, dateOfBirth = born.plusDays(1)))
        saved <- s.store.all
      yield (first, second, saved)

    test.asserting { case (first, second, saved) =>
      first.isRight shouldBe true
      second.isRight shouldBe true
      saved should have size 2
    }
  }

  private val rejected = List(
    ("an empty first name", create.copy(firstName = ""), List("firstName")),
    ("a last name of spaces", create.copy(lastName = " \t"), List("lastName")),
    (
      "both names empty",
      create.copy(firstName = "", lastName = ""),
      List("firstName", "lastName")
    )
  )

  rejected.foreach { (description, command, fields) =>
    it should s"refuse $description, naming the fields, without saving an event" in {
      val test =
        for
          s <- setup
          result <- s.run(command)
          saved <- s.store.all
        yield (result, saved)

      test.asserting { case (result, saved) =>
        result shouldBe Left(EMPTY_PATIENT_FIELDS(fields))
        saved shouldBe empty
      }
    }
  }

  it should "refuse a taken id and keep a single event" in {
    val test =
      for
        s <- setup
        first <- s.run(create)
        second <- s.run(create.copy(firstName = "Noah", lastName = "Favre"))
        saved <- s.store.all
      yield (first, second, saved)

    test.asserting { case (first, second, saved) =>
      first.isRight shouldBe true
      second shouldBe Left(PATIENT_ALREADY_EXISTS(id))
      saved.toList shouldBe first.toOption.toList.flatten
    }
  }

  it should "refuse the same person under a new id" in {
    val test =
      for
        s <- setup
        _ <- s.run(create)
        second <- s.run(create.copy(id = otherId))
        saved <- s.store.all
      yield (second, saved)

    test.asserting { case (second, saved) =>
      second shouldBe Left(PATIENT_ALREADY_EXISTS(otherId))
      saved should have size 1
    }
  }

  it should "accept only one of two concurrent creations of the same person" in {
    val test =
      for
        s <- setup
        results <- (s.run(create), s.run(create.copy(id = otherId))).parTupled
        saved <- s.store.all
      yield (results, saved)

    test.asserting { case ((first, second), saved) =>
      List(first, second).count(_.isRight) shouldBe 1
      List(first, second).collect { case Left(error) => error } should
        matchPattern { case List(PATIENT_ALREADY_EXISTS(_)) => }
      saved should have size 1
    }
  }
