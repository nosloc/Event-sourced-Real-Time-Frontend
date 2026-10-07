package prototype.client

import com.raquo.laminar.api.L.*

object HomeView:
  def apply(): HtmlElement =
    div(
      h1("Home"),
      p("Welcome to the home page!"),
      p(
        "You are logged in as: ",
        child.text <-- Session.currentUserS.map(_.getOrElse(""))
      ),
      button(
        "Logout",
        onClick --> { _ => Session.logout() }
      )
    )
