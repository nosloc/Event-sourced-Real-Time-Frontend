package prototype.client.views

import prototype.api.*
import prototype.client.{
  Api,
  AppRouter,
  Session
}
import prototype.client.Api.ApiCall
import prototype.client.Pages.{
  AdminPage,
  DashboardPage,
  EventSourcingPage,
  HomePage,
  Page,
  TimetablePage
}
import prototype.client.live.SyncChecks
import prototype.client.views.components.UserInfo

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
      renderLink("Event sourcing", EventSourcingPage)
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
