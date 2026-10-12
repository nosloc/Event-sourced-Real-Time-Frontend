package prototype.server.core.company

import cats.effect.*
import prototype.server.eventStore.*
import prototype.api.*
import prototype.server.core.company.companyModel.*

trait CompanyRepository:
  def alreadyExists(companyName: String): IO[Boolean]
  def getAllCompanies(): IO[List[Company]]

final case class CompanyViewer(
    view: Ref[IO, Map[String, Company]]
) extends Viewer[CompanyState, CompanyEvent]
    with CompanyRepository:

  override def evolve(
      state: CompanyState,
      event: CompanyEvent
  ): CompanyState =
    event match
      case CompanyCreated(companyId, name, address) =>
        Some(Company(companyId, name, address))

  override def getState(tag: String): IO[CompanyState] =
    view.get.map(_.get(tag))

  override def updateState(
      tag: String,
      events: List[CompanyEvent]
  ): IO[CompanyState] =
    view
      .updateAndGet(all =>
        val currentState = all.get(tag)
        val newState = events.foldLeft(currentState)(evolve)
        newState match
          case Some(company) => all.updated(tag, company)
          case None          => all - tag
      )
      .map(_.get(tag))

  override def alreadyExists(companyName: String): IO[Boolean] =
    view.get.map(_.values.exists(_.name == companyName))

  override def getAllCompanies(): IO[List[Company]] =
    view.get.map(_.values.toList)
