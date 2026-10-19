package prototype.client.views

import com.raquo.laminar.api.L.*
import prototype.client.Pages.Page
import prototype.client.AppRouter
import prototype.client.Pages.{
  HomePage,
  TimetablePage,
  AdminPage,
  DashboardPage
}
import prototype.client.Session
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.api.*
import prototype.client.views.components.UserInfo

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
      renderLink("Admin", AdminPage)
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
      UserInfo(currentUser),
      button(
        cls("btn"),
        "Logout",
        onClick --> { _ =>
          Session.logout()
        }
      )
    )
