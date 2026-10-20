package prototype.server.core.company

import cats.effect.*

import prototype.api.*

/** A company service that keeps its companies in memory, for the tests of the
  * services that only need a company to exist (therapists, dashboards). The
  * real one stores events in Postgres and reads from a projection.
  */
final class InMemoryCompanyService private (
    companies: Ref[IO, Map[CompanyId, Company]]
) extends CompanyService[IO]:

  override def createCompany(
      name: String,
      address: Address
  ): IO[CreateCompanyOutput] =
    IO.randomUUID.flatMap { uuid =>
      val id = CompanyId(uuid)
      companies
        .update(_ + (id -> Company(id, name, address)))
        .as(CreateCompanyOutput(id))
    }

  override def listCompanies(): IO[ListCompaniesOutput] =
    companies.get.map(all => ListCompaniesOutput(all.values.toList))

  override def getCompany(companyId: CompanyId): IO[GetCompanyOutput] =
    companies.get.map(_.get(companyId)).flatMap {
      case Some(company) => IO.pure(GetCompanyOutput(company))
      case None          => IO.raiseError(CompanyNotFound())
    }

object InMemoryCompanyService:
  def apply(): IO[CompanyService[IO]] =
    Ref.of[IO, Map[CompanyId, Company]](Map.empty).map(new InMemoryCompanyService(_))
