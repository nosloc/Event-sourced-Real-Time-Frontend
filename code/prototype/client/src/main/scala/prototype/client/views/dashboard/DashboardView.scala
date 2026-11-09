package prototype.client.views.dashboard

import java.util.UUID

import prototype.api.*
import prototype.client.companies.LiveCompanies
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.views.components.PopoverPicker
import prototype.client.views.components.PopoverPicker.Item

import com.raquo.laminar.api.L.*

object DashboardView:
  enum DashboardType:
    case BILLING
    case ACTIVITY
    case DIRECTORY

  def apply(): HtmlElement =
    val feed: LiveFeed = new LiveFeed
    val selectedCompanyVar: Var[Option[UUID]] = Var(None)
    val companies: LiveData[List[Company]] = LiveCompanies(feed)
    val selectedDashboardVar: Var[Option[DashboardType]] = Var(None)
    val titleS: Signal[String] = selectedDashboardVar.signal.map {
      case None                          => "No Selected Dashboard"
      case Some(DashboardType.BILLING)   => "Billing Dashboard"
      case Some(DashboardType.ACTIVITY)  => "Activity Dashboard"
      case Some(DashboardType.DIRECTORY) => "Directory Dashboard"
    }
    div(
      cls := "dashboards",
      feed.bind(),
      h1("Dashboards"),
      div(
        cls := "dashboards__layout",
        renderDashboardSelector(selectedDashboardVar),
        div(
          cls := "dashboards__main",
          div(
            cls := "dashboards__head",
            h2(child.text <-- titleS, cls := "dashboards__title"),
            renderCompanySelector(companies.valueS, selectedCompanyVar)
          ),
          child <-- selectedDashboardVar.signal.map {
            case None                => renderNoDashboardSelected()
            case Some(dashboardType) =>
              dashboardType match
                case DashboardType.BILLING =>
                  BillingDashboard(feed, selectedCompanyVar.signal)
                case DashboardType.ACTIVITY =>
                  ActivityDashboard(feed, selectedCompanyVar.signal)
                case DashboardType.DIRECTORY =>
                  DirectoryDashboard(feed, selectedCompanyVar.signal)
          }
        )
      )
    )
  end apply

  private def renderNoDashboardSelected(): HtmlElement =
    div(
      cls := "dashboard",
      p(
        cls := "dash__empty",
        "This is the dashboard view. Please select a dashboard from the side bar."
      )
    )
  end renderNoDashboardSelected

  private def renderCompanySelector(
      companiesS: Signal[Option[List[Company]]],
      selectedCompanyVar: Var[Option[UUID]]
  ): HtmlElement =
    PopoverPicker(
      id = "company-selector",
      label = "Select Company",
      items = companiesS
        .map(
          _.map(_.map(c => Item[Option[UUID]](Some(c.companyId.value), c.name)))
            .getOrElse(Nil)
        ),
      selectedVar = selectedCompanyVar,
      fallbackLabel = _ => "--"
    )
  end renderCompanySelector

  private def renderDashboardSelector(
      selectedDashboardVar: Var[Option[DashboardType]]
  ): HtmlElement =
    ul(
      cls := "dashboards__list panel",
      renderDashboardItemSelector(
        DashboardType.BILLING,
        selectedDashboardVar,
        "Billing",
        "View billing information for the selected company."
      ),
      renderDashboardItemSelector(
        DashboardType.ACTIVITY,
        selectedDashboardVar,
        "Activity",
        "View activity information for the selected company."
      ),
      renderDashboardItemSelector(
        DashboardType.DIRECTORY,
        selectedDashboardVar,
        "Directory",
        "View directory information for the selected company."
      )
    )
  end renderDashboardSelector

  private def renderDashboardItemSelector(
      dashboardType: DashboardType,
      selectedDashboardVar: Var[Option[DashboardType]],
      name: String,
      blurb: String
  ): HtmlElement =
    li(
      button(
        cls := "dashboards__item",
        typ := "button",
        cls("dashboards__item--active") <-- selectedDashboardVar.signal
          .map(
            _.contains(dashboardType)
          ),
        span(cls := "dashboards__item-label", name),
        span(cls := "dashboards__item-blurb", blurb),
        onClick.mapTo(Some(dashboardType)) --> selectedDashboardVar.writer
      )
    )
