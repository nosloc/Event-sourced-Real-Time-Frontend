package prototype.server

import cats.effect.*
import org.http4s.*
import org.http4s.dsl.io.*
import com.comcast.ip4s.*
import cats.implicits.*
import prototype.api.*
import org.http4s.ember.server.EmberServerBuilder

import prototype.Greeting

object Server extends IOApp.Simple:

  private val greetingRoute = HttpRoutes
    .of[IO] { case GET -> Root / "api" / "hello" =>
      Ok(Greeting.greet("World !"))
    }

  private val companyServiceImpl = new CompanyServiceImpl(Seed.companies)
  private val therapistServiceImpl = new TherapistServiceImpl(Seed.therapists)
  private val appointmentServiceImpl = new AppointmentServiceImpl(
    Seed.appointments,
    therapistServiceImpl
  )

  private val server =
    for
      tokens <- Resource.eval(Ref.of[IO, Map[Token, TherapistId]](Map.empty))
      authServiceImpl = new AuthServiceImpl(tokens, therapistServiceImpl)
      routes <- Routes.all(
        greetingRoute,
        companyServiceImpl,
        therapistServiceImpl,
        appointmentServiceImpl,
        authServiceImpl
      )
      server <- EmberServerBuilder
        .default[IO]
        .withHost(host"127.0.0.1")
        .withPort(port"9000")
        .withHttpApp(routes.orNotFound)
        .build
    yield server

  def run: IO[Unit] = server.useForever
