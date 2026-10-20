package prototype.server.core.company.commands

import java.util.UUID

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.domain.{
  CompanyCreated,
  CompanyError,
  CompanyScopes
}
import prototype.server.core.company.domain.CompanyError.*

import persistent4s.EventSourcedCommandHandler

/** Creates a new company with the given name and address
  *
  * @param id
  *   the unique identifier of the company
  * @param name
  *   the name of the company
  * @param address
  *   the address of the company
  */

final case class CreateCompany(
    id: UUID,
    name: String,
    address: Address
)

object CreateCompany:

  object Handler
      extends EventSourcedCommandHandler[
        CreateCompany,
        Boolean,
        PrototypeEvent,
        CompanyError
      ]:

    override protected val behavior = handler(initial = false):
      scope(CompanyScopes.Company)(_.id)

      on[CompanyCreated].allEvents.evolve(_ => true)

      reject:
        case (true, command) =>
          COMPANY_ALREADY_EXISTS(command.id)
        case (_, command) if command.name.trim.isEmpty =>
          COMPANY_ALREADY_EXISTS(command.id)
        case (_, command) if !isAddressValid(command.address) =>
          COMPANY_ADDRESS_INVALID(command.id)

      emit: command =>
        CompanyCreated(
          command.id,
          command.name,
          command.address.street,
          command.address.number,
          command.address.city,
          command.address.state
        )

  private def isAddressValid(address: Address): Boolean =
    address match
      case Address(street, city, postalCode, country) =>
        street.trim.nonEmpty &&
        city.trim.nonEmpty &&
        postalCode.trim.nonEmpty &&
        country.trim.nonEmpty
