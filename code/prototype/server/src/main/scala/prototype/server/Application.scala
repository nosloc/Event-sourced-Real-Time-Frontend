package prototype.server

import cats.effect.{IO, IOApp}
import org.http4s.*
import org.http4s.dsl.io.*
import com.comcast.ip4s.*
import cats.implicits.*
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

  private val routes = Routes.all(
    greetingRoute,
    companyServiceImpl,
    therapistServiceImpl,
    appointmentServiceImpl
  )

  def run: IO[Unit] =
    routes
      .flatMap(routes =>
        EmberServerBuilder
          .default[IO]
          .withHost(host"127.0.0.1")
          .withPort(port"9000")
          .withHttpApp(routes.orNotFound)
          .build
      )
      .useForever
