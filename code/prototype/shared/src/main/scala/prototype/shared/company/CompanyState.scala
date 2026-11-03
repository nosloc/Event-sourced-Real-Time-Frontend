package prototype.shared.company

import java.util.UUID

/** A company as the projection stores it and the browser folds it (see
  * `CompanyEvolve`).
  */
final case class CompanyState(
    companyId: UUID,
    name: String,
    addressStreet: String,
    addressNumber: String,
    addressCity: String,
    addressState: String
)
