package prototype.shared.patient

import java.time.LocalDate
import java.util.UUID

/** A patient as the projection stores it and the browser folds it (see
  * `PatientEvolve`).
  */
final case class PatientState(
    patientId: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
)
