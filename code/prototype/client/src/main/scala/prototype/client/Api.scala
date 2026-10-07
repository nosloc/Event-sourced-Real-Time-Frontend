package prototype.client

import org.scalajs.dom
import org.http4s.Uri
import prototype.api.*
import smithy4s.http4s.SimpleRestJsonBuilder
import cats.effect.*
import org.http4s.dom.FetchClientBuilder
import cats.effect.unsafe.implicits.global
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.global
import prototype.client.auth.AuthMiddleware

import com.raquo.laminar.api.L.*

object Api:

  private val origin = Uri.unsafeFromString(dom.window.location.origin)
  private val authMiddleware = new AuthMiddleware

  val therapists: TherapistService[IO] =
    SimpleRestJsonBuilder(TherapistService)
      .client(FetchClientBuilder[IO].create)
      .uri(origin)
      .middleware(authMiddleware)
      .make
      .fold(throw _, identity)

  val auth: AuthService[IO] =
    SimpleRestJsonBuilder(AuthService)
      .client(FetchClientBuilder[IO].create)
      .uri(origin)
      .middleware(authMiddleware)
      .make
      .fold(throw _, identity)

  def stream[A](io: IO[A]): EventStream[Either[Throwable, A]] =
    EventStream.fromFuture(io.attempt.unsafeToFuture())
