package prototype.client.views.dashboard

import java.util.UUID

import prototype.api.*
import prototype.client.companies.LiveCompanies
import prototype.client.dashboards.{ComputationLevel, DashboardSources}
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.therapists.LiveTherapists
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

    val computationLevelVar: Var[ComputationLevel] = Var(
      ComputationLevel.Browser
    )

    val stepByStepVar: Var[Boolean] = feed.tracer.enabled
    // The stepper of the dashboard shown, on its level, for the panel
    val stepperVar: Var[Option[StepByStep[?]]] = Var(None)

    // The therapists of every company, loaded once for the page
    val allTherapists: LiveData[List[Therapist]] = LiveTherapists(feed)

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
            div(
              cls := "dashboards__pickers",
              renderComputationLevelSelector(computationLevelVar),
              renderCompanySelector(companies.valueS, selectedCompanyVar)
            )
          ),
          child <-- selectedCompanyVar.signal.splitOption(
            (_, companyIdS) =>
              renderCompanyDashboards(
                DashboardSources.from(feed, allTherapists, companyIdS),
                feed,
                companyIdS,
                computationLevelVar.signal,
                selectedDashboardVar,
                stepperVar.writer
              ),
            ifEmpty = div(
              child <-- selectedDashboardVar.signal.map {
                case None    => renderNoDashboardSelected()
                case Some(_) =>
                  p(
                    cls := "dash__empty",
                    "Select a company to see the ",
                    text <-- titleS.map(_.toLowerCase()),
                    "."
                  )
              }
            )
          )
        )
      ),
      // The bottom sheet: its handle stays at the bottom of the window, and
      // opens the step-by-step panel above it
      div(
        cls := "sheet",
        cls("sheet--open") <-- stepByStepVar.signal,
        renderStepByStepButton(stepByStepVar),
        child.maybe <-- stepByStepVar.signal
          .combineWith(stepperVar.signal)
          .map((stepByStep, stepper) =>
            Option.when(stepByStep)(
              stepper match
                case Some(stepper) => stepper.panel
                case None          =>
                  div(
                    cls := "sheet__message",
                    "Select a company and a dashboard to step through"
                  )
            )
          )
      )
    )
  end apply

  // Built once a company is selected, and kept while it changes and while the
  // dashboards change: the sources load once per company
  private def renderCompanyDashboards(
      sources: DashboardSources,
      feed: LiveFeed,
      companyIdS: Signal[UUID],
      computationLevelS: Signal[ComputationLevel],
      selectedDashboardVar: Var[Option[DashboardType]],
      stepperObserver: Observer[Option[StepByStep[?]]]
  ): HtmlElement =
    div(
      // On the browser level, keeps the sources running while the dashboards
      // are swapped, so they are not loaded again; on the server level, lets
      // them stop (nothing computes from them)
      sources.keepLoadedWhile(
        computationLevelS.map(_ == ComputationLevel.Browser)
      ),
      child <-- selectedDashboardVar.signal.map {
        case None                        => renderNoDashboardSelected()
        case Some(DashboardType.BILLING) =>
          BillingDashboard(
            sources,
            feed,
            companyIdS,
            computationLevelS,
            stepperObserver
          )
        case Some(DashboardType.DIRECTORY) =>
          DirectoryDashboard(
            sources,
            feed,
            companyIdS,
            computationLevelS,
            stepperObserver
          )
        case Some(DashboardType.ACTIVITY) =>
          ActivityDashboard(
            sources,
            feed,
            companyIdS,
            computationLevelS,
            stepperObserver
          )
      }
    )
  end renderCompanyDashboards

  private def renderNoDashboardSelected(): HtmlElement =
    div(
      cls := "dashboard",
      p(
        cls := "dash__empty",
        "This is the dashboard view. Please select a dashboard from the side bar."
      )
    )
  end renderNoDashboardSelected

  private def renderComputationLevelSelector(
      computationLevelVar: Var[ComputationLevel]
  ): HtmlElement =
    PopoverPicker(
      id = "computation-level-selector",
      label = "Computed by",
      items = Val(
        List(
          Item(ComputationLevel.Browser, "Browser"),
          Item(ComputationLevel.Server, "Server")
        )
      ),
      selectedVar = computationLevelVar
    )
  end renderComputationLevelSelector

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

  private def renderStepByStepButton(
      stepByStepVar: Var[Boolean]
  ): HtmlElement =
    button(
      typ := "button",
      cls := "sheet__handle",
      aria.expanded <-- stepByStepVar.signal,
      "Step by step",
      svg.svg(
        svg.cls := "sheet__chevron",
        svg.width := "12",
        svg.height := "12",
        svg.viewBox := "0 0 12 12",
        svg.fill := "none",
        aria.hidden := true,
        svg.path(svg.d := "M2.5 7.5 6 4l3.5 3.5")
      ),
      onClick.-->(_ => stepByStepVar.update(!_))
    )
