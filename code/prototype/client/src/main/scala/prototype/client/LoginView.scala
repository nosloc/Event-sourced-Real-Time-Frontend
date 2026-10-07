package prototype.client

import com.raquo.laminar.api.L.{*, given}

object LoginView:
  def apply(): HtmlElement =
    val usernameVar = Var("")
    div(
      h1("Login"),
      input(
        placeholder("Username"),
        controlled(
          value <-- usernameVar.signal,
          onInput.mapToValue --> usernameVar.writer
        )
      ),
      button(
        "Login",
        onClick --> { _ =>
          Session.login(usernameVar.now())
          AppRouter.replaceState(Pages.HomePage)
        },
        disabled <-- usernameVar.signal.map(_.trim.isEmpty)
      )
    )
