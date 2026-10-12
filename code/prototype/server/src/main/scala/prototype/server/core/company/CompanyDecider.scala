package prototype.server.core.company

import prototype.server.core.company.companyModel.*
import prototype.server.eventStore.*
import prototype.api.*

class CompanyDecider
    extends Decider[
      CompanyState,
      CompanyCommand,
      CompanyEvent,
      CompanyError
    ]:

  def initialState: CompanyState = None

  def decide(
      state: CompanyState,
      command: CompanyCommand
  ): Either[CompanyError, List[CompanyEvent]] =
    command match
      case CreateCompanyCommand(companyId, name, address) =>
        state match
          case None =>
            Right(List(CompanyCreated(companyId, name, address)))
          case Some(_) =>
            Left(ALREADY_EXISTS)

  def evolve(
      state: CompanyState,
      event: CompanyEvent
  ): CompanyState =
    event match
      case CompanyCreated(companyId, name, address) =>
        Some(Company(companyId, name, address))
