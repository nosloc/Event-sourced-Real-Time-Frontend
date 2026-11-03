package prototype.shared.therapist

import java.util.UUID

/** A therapist as the projection stores it and the browser folds it (see
  * `TherapistEvolve`). Plain types only: the smithy types (`TherapistId`,
  * `CompanyId`, `Role`) cannot be mapped to columns, so the role is kept as its
  * name.
  */
final case class TherapistState(
    therapistId: String,
    firstName: String,
    lastName: String,
    companyId: UUID,
    role: String
)
