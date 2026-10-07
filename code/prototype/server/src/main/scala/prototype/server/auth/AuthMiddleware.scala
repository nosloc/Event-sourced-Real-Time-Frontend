package prototype.server.auth

import cats.effect.*
import prototype.api.*
import smithy4s.http4s.ServerEndpointMiddleware
import smithy4s.Hints
import org.http4s.HttpApp
import org.http4s.headers.Authorization
import org.http4s.Credentials
import org.http4s.AuthScheme

class AuthMiddleware(authChecker: AuthChecker)
    extends ServerEndpointMiddleware.Simple[IO]:

  override def prepareWithHints(
      serviceHints: Hints,
      endpointHints: Hints
  ): HttpApp[IO] => HttpApp[IO] =
    val mid = middleware
    serviceHints.get[smithy.api.HttpBearerAuth] match
      case Some(_) =>
        endpointHints.get[smithy.api.Auth] match
          case Some(auths) if auths.value.isEmpty => identity
          case _                                  => mid
      case None => identity
  end prepareWithHints

  private def middleware: HttpApp[IO] => HttpApp[IO] = { inputApp =>
    HttpApp[IO] { request =>
      val keyOpt = request.headers
        .get[Authorization]
        .collect {
          case Authorization(Credentials.Token(AuthScheme.Bearer, token)) =>
            token
        }

      val isAuthorized = keyOpt match
        case Some("ac709fa8-aee3-4e87-b120-14c2c410873a") =>
          IO.pure(true) // Hardcoded token for testing purposes
        case Some(token) =>
          authChecker.userFor(token).map(_.isDefined)
        case None => IO.pure(false)
      isAuthorized.ifM(
        ifTrue = inputApp(request),
        ifFalse = IO.raiseError(Unauthorized())
      )
    }
  }
  end middleware
end AuthMiddleware
