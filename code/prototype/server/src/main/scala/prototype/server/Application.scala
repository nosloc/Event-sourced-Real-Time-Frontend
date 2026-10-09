package prototype.server

import cats.effect.*
import org.http4s.*
import org.http4s.dsl.io.*
import com.comcast.ip4s.*
import prototype.api.*
import org.http4s.ember.server.EmberServerBuilder

import prototype.Greeting
import prototype.server.auth.*

object Server extends IOApp.Simple:

  private val greetingRoute = HttpRoutes
    .of[IO] { case GET -> Root / "api" / "hello" =>
      Ok(Greeting.greet("World !"))
    }

  // Bind host: loopback for local dev, overridden (0.0.0.0) inside the container
  private val bindHost: IO[Host] =
    IO(sys.env.get("SERVER_HOST")).flatMap:
      case None => IO.pure(host"127.0.0.1")
      case Some(raw) =>
        IO.fromOption(Host.fromString(raw))(
          new IllegalArgumentException(s"Invalid SERVER_HOST: $raw")
        )

  private val server =
    for
      bindHost <- Resource.eval(bindHost)
      seed <- Resource.eval(Seed())
      companyServiceImpl = new CompanyServiceImpl(seed.companies)
      therapistServiceImpl = new TherapistServiceImpl(seed.therapists)
      appointmentServiceImpl = new AppointmentServiceImpl(
        seed.appointments,
        therapistServiceImpl
      )
      tokens <- Resource.eval(Ref.of[IO, Map[Token, TherapistId]](Map.empty))
      authServiceImpl = new AuthServiceImpl(tokens, therapistServiceImpl)
      authChecker = AuthChecker(tokens)
      middleware = new AuthMiddleware(authChecker)
      routes <- Routes.all(
        greetingRoute,
        middleware,
        companyServiceImpl,
        therapistServiceImpl,
        appointmentServiceImpl,
        authServiceImpl
      )
      server <- EmberServerBuilder
        .default[IO]
        .withHost(bindHost)
        .withPort(port"9000")
        .withHttpApp(routes.orNotFound)
        .build
    yield server

  def run: IO[Unit] = server.useForever
