package prototype.client.companies

import prototype.api.Company
import prototype.shared.company.{
  CompanyEvent,
  CompanyEvolve,
  CompanyState
}

import org.scalajs.dom

/** The companies: applies `CompanyEvolve` to the company an event is about.
  * Companies can only be created today; the full update / delete handling is
  * kept so the list follows the fold if events are added.
  */
object CompanyListEvolve:
  def evolve(
      companies: List[Company],
      event: CompanyEvent
  ): List[Company] =
    val current = companies.find(_.companyId.value == event.id)
    val evolveResult = CompanyEvolve
      .evolve(
        current.map(CompanyState.fromCompany),
        event
      )
      .map(_.map(CompanyState.toCompany))

    evolveResult match
      case Left(error) =>
        dom.console.warn(
          s"Company event skipped, it does not fit the companies held: ${error.getMessage}"
        )
        companies // The event does not fit the current state: ignore it, keep the current state
      case Right(evolved) =>
        updateCompanies(
          companies,
          event,
          current,
          evolved
        )
  end evolve

  private def updateCompanies(
      companies: List[Company],
      event: CompanyEvent,
      current: Option[Company],
      evolved: Option[Company]
  ): List[Company] =
    evolved match
      case Some(updated) if current.isDefined =>
        companies.map(c =>
          if c.companyId.value == event.id then updated
          else c
        )
      case Some(created) =>
        created :: companies
      case None if current.isDefined =>
        companies.filterNot(_.companyId.value == event.id)
      case None =>
        companies
  end updateCompanies
