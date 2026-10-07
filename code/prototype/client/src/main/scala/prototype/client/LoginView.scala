package prototype.client

import com.raquo.laminar.api.L.*
import prototype.api.InvalidCredentials

object LoginView:
  def apply(): HtmlElement =
    val usernameVar = Var("")
    val errorVar = Var[Option[String]](None)
    val requestRunning: Var[Boolean] = Var(false)
    val invalidUserNameS: Signal[Boolean] =
      usernameVar.signal.map(_.trim.isEmpty)
    div(
      h1("Login"),
      child.maybe <-- errorVar.signal.map {
        case Some(error) => Some(div(color.red, error))
        case None        => None
      },
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
          requestRunning.set(true)
          errorVar.set(None)
        },
        onClick
          .mapTo(usernameVar.now())
          .flatMap(name =>
            Api.stream(Api.auth.login(name)).map((name, _))
          ) --> { (name, response) =>
          response match
            case Right(answer) =>
              Session.login(name, answer.token)
              AppRouter.replaceState(Pages.HomePage)
            case Left(_: InvalidCredentials) =>
              errorVar.set(Some(s"Unknown user: $name"))
            case Left(error) =>
              errorVar.set(Some(s"Login failed: ${error.getMessage}"))
          requestRunning.set(false)
        },
        disabled <-- invalidUserNameS
          .combineWith(requestRunning.signal)
          .map(_ || _)
      )
    )
