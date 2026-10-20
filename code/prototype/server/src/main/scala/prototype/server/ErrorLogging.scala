package prototype.server

import cats.data.Kleisli
import cats.effect.*

import fs2.Stream

import org.http4s.*
import org.http4s.server.middleware.ErrorAction
import org.typelevel.ci.*

/** Prints errors to the server's terminal, which the client otherwise only
  * sees: every response with a status of 400 or more (with the error type the
  * API sets and the start of the body), and the stack trace of anything that
  * went wrong without being turned into a response.
  */
object ErrorLogging:
  private val errorTypeHeader = ci"X-Error-Type"
  private val maxBodyLength = 300

  def apply(app: HttpApp[IO]): HttpApp[IO] =
    ErrorAction.httpApp(logErrorResponses(app), logException)

  private def logErrorResponses(app: HttpApp[IO]): HttpApp[IO] =
    Kleisli { request =>
      app.run(request).flatMap { response =>
        if response.status.code < 400 then IO.pure(response)
        else
          // The body is read to print it, so it is put back for the client
          response.bodyText.compile.string.flatMap { body =>
            val errorType = response.headers
              .get(errorTypeHeader)
              .map(_.head.value)
              .fold("")(t => s" $t")
            val shownBody =
              if body.isEmpty then "" else s" ${body.take(maxBodyLength)}"
            IO.println(
              s"[error] ${request.method} ${request.uri} -> ${response.status.code}$errorType$shownBody"
            ).as(
              response.withBodyStream(
                Stream.emit(body).through(fs2.text.utf8.encode)
              )
            )
          }
      }
    }

  private def logException(request: Request[IO], error: Throwable): IO[Unit] =
    IO.println(
      s"[error] ${request.method} ${request.uri} raised ${error.getClass.getSimpleName}: ${error.getMessage}"
    ) *> IO.blocking(error.printStackTrace())
