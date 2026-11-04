package prototype.shared.company

import java.util.UUID
import prototype.api.{Company, CompanyId, Address}

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

/** The state and the API's `Company` are the same data in two shapes: the
  * state uses a plain id and flat address fields (as stored and folded), the
  * API its smithy wrappers (`CompanyId`, `Address`).
  */
object CompanyState:

  def toCompany(state: CompanyState): Company =
    Company(
      companyId = CompanyId(state.companyId),
      name = state.name,
      address = Address(
        street = state.addressStreet,
        number = state.addressNumber,
        city = state.addressCity,
        state = state.addressState
      )
    )

  def fromCompany(company: Company): CompanyState =
    CompanyState(
      companyId = company.companyId.value,
      name = company.name,
      addressStreet = company.address.street,
      addressNumber = company.address.number,
      addressCity = company.address.city,
      addressState = company.address.state
    )
