package prototype.client.Views

import com.raquo.laminar.api.L.*
import prototype.client.Pages.Page
import prototype.client.AppRouter
import prototype.client.Pages.HomePage
import prototype.client.Pages.TimetablePage
import prototype.client.Session

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
      renderLink("Timetable", TimetablePage)
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
    div(
      cls("topbar__user"),
      span(
        username
      ),
      button(
        cls("btn"),
        "Logout",
        onClick --> { _ =>
          Session.logout()
        }
      )
    )
