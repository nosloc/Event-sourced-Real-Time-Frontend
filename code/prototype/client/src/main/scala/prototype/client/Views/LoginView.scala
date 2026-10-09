package prototype.client.Views

import com.raquo.laminar.api.L.*

import prototype.api.InvalidCredentials
import prototype.client.{Api, AppRouter, Pages, Session}

object LoginView:
  def apply(): HtmlElement =
    val usernameVar = Var("")
    val errorVar = Var[Option[String]](None)
    val requestRunning: Var[Boolean] = Var(false)
    val invalidUserNameS: Signal[Boolean] =
      usernameVar.signal.map(_.trim.isEmpty)
    mainTag(
      cls := "login",
      div(cls := "login__brand", "PDM"),
      div(
        cls := "panel login__card",
        h1("Sign in"),
        div(
          cls := "field",
          label(cls := "field__label", forId := "login-username", "Username"),
          input(
            cls := "field__input",
            idAttr := "login-username",
            placeholder("Username"),
            controlled(
              value <-- usernameVar.signal,
              onInput.mapToValue --> usernameVar.writer
            )
          ),
          child.maybe <-- errorVar.signal.map {
            case Some(error) =>
              Some(div(cls := "field__error", error))
            case None => None
          }
        ),
        button(
          cls := "btn btn--primary",
          "Sign in",
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
      ),
      p(
        cls := "login__trust muted",
        "Synthetic data only. This prototype holds no patient information."
      )
    )
