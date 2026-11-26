package prototype.server.core.company.api

import cats.effect.*

import prototype.api.*
import prototype.server.core.company.commands.CreateCompany
import prototype.server.core.company.domain.CompanyError.*
import prototype.server.core.company.persistence.{
  CompanyProjection,
  CompanyProjectionName,
  CompanyRepository
}
import prototype.server.eventfeed.ApiCursor
import prototype.server.sync.{NewestLoad, ReadModel}
import prototype.shared.company.CompanyState
import prototype.shared.events.PrototypeEvent

import persistent4s.CommandRuntime

import org.typelevel.log4cats.slf4j.Slf4jLogger

class CompanyServiceImpl(
    companies: CompanyRepository,
    load: NewestLoad
)(using CommandRuntime[IO, PrototypeEvent])
    extends CompanyService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  private val readModel =
    ReadModel(CompanyProjectionName, CompanyProjection(companies).filter)

  override def createCompany(
      name: String,
      address: Address
  ): IO[CreateCompanyOutput] =
    for
      id <- IO.randomUUID
      events <- CreateCompany.Handler.runOrRaise[IO](
        CreateCompany(id, name, address)
      ) {
        case COMPANY_ALREADY_EXISTS(_)  => CompanyAlreadyExists()
        case COMPANY_NAME_TAKEN(_)      => CompanyAlreadyExists()
        case COMPANY_NAME_EMPTY(_)      => InvalidCompany()
        case COMPANY_ADDRESS_INVALID(_) => InvalidCompany()
      }
      _ <- logger.info(s"Created company $name with id $id\n")
      cursor <- ApiCursor.required(ApiCursor.lastWritten(events))
    yield CreateCompanyOutput(CompanyId(id), cursor)

  override def listCompanies(): IO[ListCompaniesOutput] =
    load(readModel)(companies.allCompanies).map(loaded =>
      ListCompaniesOutput(
        loaded.snapshot.map(CompanyState.toCompany),
        loaded.cursor,
        loaded.catchUp
      )
    )
  override def getCompany(companyId: CompanyId): IO[GetCompanyOutput] =
    companies.find(companyId.value).flatMap {
      case Some(company) =>
        IO.pure(GetCompanyOutput(CompanyState.toCompany(company)))
      case None => IO.raiseError(CompanyNotFound())
    }
