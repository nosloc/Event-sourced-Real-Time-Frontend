package prototype.server.core.company

import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.ErrorCode

object companyModel:
  sealed trait CompanyCommand extends Command
  case class CreateCompanyCommand(
      companyId: CompanyId,
      name: String,
      address: Address
  ) extends CompanyCommand

  sealed trait CompanyEvent extends Event
  case class CompanyCreated(
      companyId: CompanyId,
      name: String,
      address: Address
  ) extends CompanyEvent

  sealed trait CompanyError extends ErrorCode
  case object ALREADY_EXISTS extends CompanyError

  type CompanyState = Option[Company]
