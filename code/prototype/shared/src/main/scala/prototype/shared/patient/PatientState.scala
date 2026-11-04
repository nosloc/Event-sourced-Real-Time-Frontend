package prototype.shared.patient

import java.time.LocalDate
import java.util.UUID

import prototype.api.{Patient, PatientId}

/** A patient as the projection stores it and the browser folds it (see
  * `PatientEvolve`).
  */
final case class PatientState(
    patientId: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
)

/** The state and the API's `Patient` are the same data in two shapes: the
  * state uses a plain id and a `java.time` date (as stored and folded), the API
  * its smithy types (`PatientId`, smithy4s' `LocalDate`). The date goes
  * through its day count: smithy4s' conversions to `java.time` exist on the JVM
  * only.
  */
object PatientState:

  def toPatient(state: PatientState): Patient =
    Patient(
      id = PatientId(state.patientId),
      firstName = state.firstName,
      lastName = state.lastName,
      dateOfBirth = smithy4s.time.LocalDate(state.dateOfBirth.toEpochDay)
    )

  def fromPatient(patient: Patient): PatientState =
    PatientState(
      patientId = patient.id.value,
      firstName = patient.firstName,
      lastName = patient.lastName,
      dateOfBirth = LocalDate.ofEpochDay(patient.dateOfBirth.epochDay)
    )
