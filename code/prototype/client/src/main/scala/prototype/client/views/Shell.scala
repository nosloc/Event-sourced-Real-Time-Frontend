package prototype.client.views

import prototype.api.*
import prototype.client.{Api, AppRouter, Session}
import prototype.client.Api.ApiCall
import prototype.client.Pages.{
  AdminPage,
  DashboardPage,
  EventSourcingPage,
  FaultsPage,
  HomePage,
  Page,
  TimetablePage
}
import prototype.client.live.SyncChecks
import prototype.client.views.components.UserInfo
import prototype.client.views.faults.{FaultsStatus, FaultsView}

import com.raquo.laminar.api.L.*

object Shell:

  def apply(innerContent: HtmlElement, username: String): HtmlElement =
    div(
      cls("shell"),
      headerTag(
        cls("topbar"),
        div(
          cls("topbar__inner"),
          renderBrandText(),
          renderNavigation(),
          renderUser(username)
        )
      ),
      mainTag(
        cls("page"),
        innerContent
      )
    )

  private def renderBrandText(): HtmlElement =
    span(
      "PDM",
      cls("topbar__brand")
    )

  private def renderNavigation(): HtmlElement =
    navTag(
      cls("topbar__nav"),
      renderLink("Home", HomePage),
      renderLink("Timetable", TimetablePage),
      renderLink("Dashboard", DashboardPage),
      renderLink("Admin", AdminPage),
      renderLink("Event sourcing", EventSourcingPage),
      renderLink("Faults", FaultsPage)
    )

  private def renderLink(label: String, page: Page): HtmlElement =
    a(
      cls("topbar__link"),
      cls("topbar__link--active") <-- AppRouter.currentPageSignal.map(
        _ == page
      ),
      label,
      AppRouter.navigateTo(page, replaceState = false)
    )

  private def renderUser(username: String): HtmlElement =
    // The logged-in therapist, shown next to the logout button
    val currentUser: ApiCall[Therapist] =
      ApiCall.fromIOCall(
        Api.therapists.getTherapist(TherapistId(username)),
        _.therapist
      )
    div(
      cls("topbar__user"),
      child.maybe <-- faultsOnS.map(Option.when(_)(renderFaultsPill())),
      renderSyncSwitch(),
      UserInfo(currentUser),
      button(
        cls("btn"),
        "Logout",
        onClick --> { _ =>
          Session.logout()
        }
      )
    )

  // Whether the server injects a fault: read once when the page is built (the
  // shell is built again on each page), then each change made on the Faults
  // page (`FaultsStatus`). Hidden while it loads, and on an error (a server
  // with faults off answers 404)
  private def faultsOnS: Signal[Boolean] =
    EventStream
      .merge(
        Api.stream(Api.faults.getFaults()).collect { case Right(faults) =>
          faults
        },
        FaultsStatus.applied
      )
      .map(FaultsView.anyOn)
      .toSignal(false)

  // Says on every page that a run is slowed on purpose; opens the Faults page
  private def renderFaultsPill(): HtmlElement =
    a(
      cls("faults-pill"),
      title := "The server adds delays on purpose: see the Faults page",
      "Faults on",
      AppRouter.navigateTo(FaultsPage, replaceState = false)
    )

  // The global switch of the checks against the server (see `SyncChecks`)
  private def renderSyncSwitch(): HtmlElement =
    label(
      cls("switch"),
      title := "Compare what each page shows with the server (sends requests)",
      input(
        typ := "checkbox",
        cls("switch__input"),
        role := "switch",
        checked <-- SyncChecks.enabledS,
        onInput.mapToChecked --> (SyncChecks.set(_))
      ),
      span(cls("switch__track"), aria.hidden := true),
      span(cls("switch__label"), "Sync checks")
    )
