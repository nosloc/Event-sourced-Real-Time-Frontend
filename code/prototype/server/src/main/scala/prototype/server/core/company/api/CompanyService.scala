package prototype.server.core.company.api

import cats.effect.*

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.commands.CreateCompany
import prototype.server.core.company.domain.CompanyError.*
import prototype.server.core.company.persistence.{
  CompanyRepository,
  CompanyState
}

import persistent4s.CommandRuntime

import org.typelevel.log4cats.slf4j.Slf4jLogger

class CompanyServiceImpl(
    companies: CompanyRepository
)(using CommandRuntime[IO, PrototypeEvent])
    extends CompanyService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  private def fromState(state: CompanyState): Company =
    Company(
      CompanyId(state.companyId),
      state.name,
      Address(
        state.addressStreet,
        state.addressNumber,
        state.addressCity,
        state.addressState
      )
    )

  override def createCompany(
      name: String,
      address: Address
  ): IO[CreateCompanyOutput] =
    for
      id <- IO.randomUUID
      _ <- CreateCompany.Handler.runOrRaise[IO](
        CreateCompany(id, name, address)
      ) {
        case COMPANY_ALREADY_EXISTS(_)  => CompanyAlreadyExists()
        case COMPANY_NAME_EMPTY(_)      => InvalidCompany()
        case COMPANY_ADDRESS_INVALID(_) => InvalidCompany()
      }
      _ <- logger.info(s"Created company $name with id $id\n")
    yield CreateCompanyOutput(CompanyId(id))

  override def listCompanies(): IO[ListCompaniesOutput] =
    companies.all.map(companies =>
      ListCompaniesOutput(companies.map(fromState))
    )

  override def getCompany(companyId: CompanyId): IO[GetCompanyOutput] =
    companies.find(companyId.value).flatMap {
      case Some(company) => IO.pure(GetCompanyOutput(fromState(company)))
      case None          => IO.raiseError(CompanyNotFound())
    }
