package prototype.client.Views

import com.raquo.laminar.api.L.*

import prototype.client.Session

object HomeView:
  def apply(username: String): HtmlElement =
    div(
      h1("Home"),
      p("Welcome to the home page!"),
      p("You are logged in as: ", username),
      button(
        "Go to Timetable",
        onClick --> { _ =>
          prototype.client.AppRouter
            .pushState(prototype.client.Pages.TimetablePage)
        }
      ),
      button(
        "Logout",
        onClick --> { _ => Session.logout() }
      )
    )
