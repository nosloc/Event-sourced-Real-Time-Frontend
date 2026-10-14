package prototype.server.core.company

import cats.effect.*
import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.core.company.companyModel.*
import java.util.UUID

trait CompanyServiceImpl extends CompanyService[IO]

object CompanyServiceImpl:
  def apply(
      eventStore: EventStore
  ): IO[CompanyServiceImpl] =
    for
      view <- Ref.of[IO, Map[String, Company]](Map.empty)
      viewer = CompanyViewer(view)
      decider = CompanyDecider()

    yield new CompanyServiceImpl:

      val handler = new CommandHandler(decider, viewer, eventStore)

      override def createCompany(
          name: String,
          address: Address
      ): IO[CreateCompanyOutput] =
        val id = UUID.randomUUID()
        val command = CreateCompanyCommand(
          CompanyId(id),
          name,
          address
        )
        IO.print(s"Creating company $name with id $id\n") *>
          handler.handle(id.toString, command).flatMap {
            case Right(company) => IO.pure(CreateCompanyOutput(company.get))
            case Left(ALREADY_EXISTS) =>
              IO.raiseError(CompanyAlreadyExists())
          }

      override def listCompanies(): IO[ListCompaniesOutput] =
        viewer.getAllCompanies().map(ListCompaniesOutput.apply)

      override def getCompany(companyId: CompanyId): IO[GetCompanyOutput] =
        viewer.getState(companyId.value.toString).flatMap {
          case Some(company) => IO.pure(GetCompanyOutput(company))
          case None          => IO.raiseError(CompanyNotFound())
        }
