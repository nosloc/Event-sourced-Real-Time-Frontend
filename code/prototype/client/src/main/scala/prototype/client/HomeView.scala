package prototype.client

import com.raquo.laminar.api.L.{*, given}

object HomeView:
  def apply(): HtmlElement =
    div(
      h1("Home"),
      p("Welcome to the home page!"),
      p(
        "You are logged in as: ",
        child <-- Session.currentUserS.map {
          case Some(username) => span(username)
          case _              =>
            span("Unknown user") // Should not happen, but just in case
        }
      ),
      button(
        "Logout",
        onClick --> { _ => Session.logout() }
      )
    )
