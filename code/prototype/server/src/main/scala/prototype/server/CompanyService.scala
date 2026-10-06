package prototype.server

import cats.effect.*
import cats.implicits.*
import prototype.api.*

class CompanyServiceImpl(companies: List[Company]) extends CompanyService[IO]:
  override def listCompanies(): IO[ListCompaniesOutput] =
    ListCompaniesOutput(companies).pure[IO]
  override def getCompany(companyId: CompanyId): IO[GetCompanyOutput] =
    companies.find(_.companyId == companyId) match
      case Some(company) => GetCompanyOutput(company).pure[IO]
      case None          => IO.raiseError(CompanyNotFound())
