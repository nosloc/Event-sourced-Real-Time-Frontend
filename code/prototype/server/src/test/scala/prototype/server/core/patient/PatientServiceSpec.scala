package prototype.server.core.patient

import java.util.UUID
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.eventStore.InMemoryEventStore
import smithy4s.time.LocalDate

/** The patient service end to end, with the real decider, viewer and store. */
class PatientServiceSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  // A fresh service (and store) per test
  private val newService: IO[PatientService[IO]] =
    InMemoryEventStore.create.flatMap(PatientServiceImpl(_))

  "createPatient" should "make the patient available through getPatient with every field" in {
    val test =
      for
        service <- newService
        created <- service.createPatient("John", "Doe", LocalDate(1990, 1, 1))
        fetched <- service.getPatient(created.patientId)
      yield (created, fetched)

    test.asserting { case (created, fetched) =>
      fetched.patient shouldBe
        Patient(created.patientId, "John", "Doe", LocalDate(1990, 1, 1))
    }
  }

  it should "give every patient its own id" in {
    val test =
      for
        service <- newService
        first <- service.createPatient("John", "Doe", LocalDate(1990, 1, 1))
        second <- service.createPatient("Marie", "Dupont", LocalDate(1985, 3, 12))
      yield (first.patientId, second.patientId)

    test.asserting { case (firstId, secondId) =>
      firstId should not be secondId
    }
  }

  "getPatient" should "fail with PatientNotFound for an unknown id" in {
    val unknown = PatientId(UUID.randomUUID())
    val test = newService.flatMap(_.getPatient(unknown))

    test.attempt.asserting(_ shouldBe Left(PatientNotFound(unknown)))
  }

  "listPatients" should "be empty before any patient is created" in {
    newService
      .flatMap(_.listPatients())
      .asserting(_.patients shouldBe Nil)
  }

  it should "list every created patient" in {
    val test =
      for
        service <- newService
        first <- service.createPatient("John", "Doe", LocalDate(1990, 1, 1))
        second <- service.createPatient("Marie", "Dupont", LocalDate(1985, 3, 12))
        listed <- service.listPatients()
      yield (first.patientId, second.patientId, listed.patients.map(_.id))

    test.asserting { case (firstId, secondId, listedIds) =>
      listedIds should contain theSameElementsAs List(firstId, secondId)
    }
  }

  it should "sort by last name, then first name, whatever the creation order" in {
    // Created out of order: a map would not keep any order, and the list is
    // shown to users
    val test =
      for
        service <- newService
        _ <- service.createPatient("Sophie", "Durand", LocalDate(1992, 5, 5))
        _ <- service.createPatient("Marie", "Dupont", LocalDate(1985, 3, 12))
        _ <- service.createPatient("Alice", "Dupont", LocalDate(1979, 7, 30))
        listed <- service.listPatients()
      yield listed.patients.map(p => s"${p.firstName} ${p.lastName}")

    test.asserting(_ shouldBe List("Alice Dupont", "Marie Dupont", "Sophie Durand"))
  }
