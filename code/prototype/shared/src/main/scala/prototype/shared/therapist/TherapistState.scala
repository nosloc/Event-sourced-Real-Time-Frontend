package prototype.shared.therapist

import java.util.UUID

import prototype.api.Role

/** A therapist as the projection stores it and the browser folds it (see
  * `TherapistEvolve`). Plain ids: the smithy wrappers (`TherapistId`,
  * `CompanyId`) are not stored; the role is stored by its name (see
  * `TherapistRepository`).
  */
final case class TherapistState(
    therapistId: String,
    firstName: String,
    lastName: String,
    companyId: UUID,
    role: Role
)
