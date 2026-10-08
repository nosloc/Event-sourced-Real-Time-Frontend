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
  sealed trait ApiState[+R]
  case object Loading extends ApiState[Nothing]
  case class Success[+R](result: R) extends ApiState[R]
  case class Error(error: Throwable) extends ApiState[Nothing]

  case class ApiCall[R](
      stateS: Signal[ApiState[R]]
  ):
    def loadingS: Signal[Boolean] = stateS.map {
      case Loading => true
      case _       => false
    }
    def successS: Signal[Option[R]] = stateS.map {
      case Success(result) => Some(result)
      case _               => None
    }
    def errorS: Signal[Option[Throwable]] = stateS.map {
      case Error(error) => Some(error)
      case _            => None
    }
  object ApiCall:
    def fromIOCall[R, TR](
        io: IO[R],
        successTransform: R => TR
    ): ApiCall[TR] =
      val responseS: EventStream[Either[Throwable, R]] = Api.stream(io)
      val stateS: Signal[ApiState[TR]] = responseS
        .map {
          case Right(result) => Success(successTransform(result))
          case Left(error)   => Error(error)
        }
        .toSignal(Loading)
      ApiCall(stateS)
    def fromIOCallSimple[R](io: IO[R]): ApiCall[R] =
      fromIOCall(io, identity)

  private val origin = Uri.unsafeFromString(dom.window.location.origin)
  private val authMiddleware = new AuthMiddleware

  val therapists: TherapistService[IO] =
    SimpleRestJsonBuilder(TherapistService)
      .client(FetchClientBuilder[IO].create)
      .uri(origin)
      .middleware(authMiddleware)
      .make
      .fold(throw _, identity)

  val appointments: AppointmentService[IO] =
    SimpleRestJsonBuilder(AppointmentService)
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
