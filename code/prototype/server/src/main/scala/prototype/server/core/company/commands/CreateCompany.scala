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

  /** What the existing companies already use
    *
    * @param id
    *   a company has the command's id
    * @param name
    *   a company has the command's name
    */
  final case class Taken(id: Boolean, name: Boolean)

  object Handler
      extends EventSourcedCommandHandler[
        CreateCompany,
        Taken,
        PrototypeEvent,
        CompanyError
      ]:

    override protected val behavior =
      handler(initial = Taken(id = false, name = false)):
        scope(CompanyScopes.Company)(_.id)
        scope(CompanyScopes.CompanyUnique)(_ => ())

        on[CompanyCreated].allEvents.evolve((taken, command, event) =>
          Taken(
            id = taken.id || event.id == command.id,
            name = taken.name || sameName(event.name, command.name)
          )
        )

        // The id first: recreating the same company (the dev account at every
        // boot) is still refused as "already exists"
        reject:
          case (taken, command) if taken.id =>
            COMPANY_ALREADY_EXISTS(command.id)
          case (_, command) if command.name.trim.isEmpty =>
            COMPANY_NAME_EMPTY(command.id)
          case (_, command) if !isAddressValid(command.address) =>
            COMPANY_ADDRESS_INVALID(command.id)
          case (taken, command) if taken.name =>
            COMPANY_NAME_TAKEN(command.name)

        emit: command =>
          CompanyCreated(
            command.id,
            command.name,
            command.address.street,
            command.address.number,
            command.address.city,
            command.address.state
          )

  // Spaces around the name and letter case do not make another name
  private def sameName(a: String, b: String): Boolean =
    a.trim.equalsIgnoreCase(b.trim)

  private def isAddressValid(address: Address): Boolean =
    address match
      case Address(street, city, postalCode, country) =>
        street.trim.nonEmpty &&
        city.trim.nonEmpty &&
        postalCode.trim.nonEmpty &&
        country.trim.nonEmpty
