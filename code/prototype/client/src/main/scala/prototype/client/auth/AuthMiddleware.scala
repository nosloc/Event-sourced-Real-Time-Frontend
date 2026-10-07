package prototype.client.auth

import cats.effect.*
import prototype.api.*
import smithy4s.http4s.*
import smithy4s.Hints
import org.http4s.client.Client
import prototype.client.Session
import org.http4s.headers.Authorization
import org.http4s.Credentials
import org.http4s.AuthScheme
import org.http4s.Status

case class NoTokenFoundException()
    extends Exception("No token found in session")

class AuthMiddleware extends ClientEndpointMiddleware.Simple[IO]:
  override def prepareWithHints(
      serviceHints: Hints,
      endpointHints: Hints
  ): Client[IO] => Client[IO] =
    val mid = middleware
    serviceHints.get[smithy.api.HttpBearerAuth] match
      case Some(_) =>
        endpointHints.get[smithy.api.Auth] match
          case Some(auths) if auths.value.isEmpty => identity
          case _                                  => mid
      case None => identity
  end prepareWithHints

  private def middleware: Client[IO] => Client[IO] = { inputClient =>
    Client[IO] { request =>
      for
        // read the token when the request is made, not when the client is built
        token <- Resource.eval(
          IO.delay(Session.currentToken)
            .flatMap(IO.fromOption(_)(NoTokenFoundException()))
        )
        authHeader = Authorization(
          Credentials.Token(AuthScheme.Bearer, token.value)
        )
        response <- inputClient.run(request.putHeaders(authHeader)).evalTap {
          r =>
            if r.status == Status.Unauthorized then IO.delay(Session.logout())
            else IO.unit
        }
      yield response
    }
  }
  end middleware
end AuthMiddleware
