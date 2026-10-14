package prototype.server.core.patient

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.patient.patientModel.*
import smithy4s.time.LocalDate

/** A patient has a single command (create). The server generates the id, so
  * the decider only turns a valid command into an event and applies it.
  */
class PatientDeciderSpec extends AnyFlatSpec with Matchers:

  private val decider = PatientDecider()

  private val id = PatientId(UUID.fromString("3b1c6b52-0a39-4f43-9d6a-1b8e0a7c9d11"))
  private val birthday = LocalDate(1990, 1, 1)
  private val create = CreatePatientCommand(id, "John", "Doe", birthday)

  "decide" should "produce PatientCreated when the patient does not exist" in {
    decider.decide(None, create) shouldBe
      Right(List(PatientCreated(id, "John", "Doe", birthday)))
  }

  "evolve" should "build the patient from PatientCreated, each field in its place" in {
    // Distinct values everywhere, so a swapped field would show
    val other = PatientId(UUID.fromString("7d0f2a64-5c1e-4b8a-8f3e-2c4d6e8f0a12"))

    decider.evolve(None, PatientCreated(other, "Marie", "Dupont", LocalDate(1985, 3, 12))) shouldBe
      Some(Patient(other, "Marie", "Dupont", LocalDate(1985, 3, 12)))
  }

  "the events produced by decide" should "build the expected state when applied with evolve" in {
    val state = decider
      .decide(None, create)
      .map(_.foldLeft(Option.empty[Patient])(decider.evolve))

    state shouldBe Right(Some(Patient(id, "John", "Doe", birthday)))
  }

  "the decider" should "start from no patient" in {
    decider.initialState shouldBe None
  }
