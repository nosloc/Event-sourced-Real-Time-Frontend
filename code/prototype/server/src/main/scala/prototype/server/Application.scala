package prototype.server

import cats.effect.*

import prototype.Greeting
import prototype.api.*
import prototype.server.auth.*
import prototype.server.config.AppConfig
import prototype.server.config.syntax.loadIO
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.*
import prototype.server.core.company.api.CompanyServiceImpl
import prototype.server.core.company.persistence.{
  CompanyProjection,
  CompanyRepository
}
import prototype.server.core.dashboards.*
import prototype.server.core.patient.*
import prototype.server.core.therapist.*
import prototype.server.eventStore.*

import persistent4s.postgres.PostgresModule

import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.{Meter, MeterProvider}
import org.typelevel.otel4s.trace.{Tracer, TracerProvider}
import pureconfig.ConfigSource

given Tracer[IO] = Tracer.Implicits.noop
given Meter[IO] = Meter.Implicits.noop
private given TracerProvider[IO] = TracerProvider.noop[IO]
private given MeterProvider[IO] = MeterProvider.noop[IO]

object App extends IOApp.Simple:

  private given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val greetingRoute = HttpRoutes
    .of[IO] { case GET -> Root / "api" / "hello" =>
      Ok(Greeting.greet("World !"))
    }

  private val program =
    for
      config <- ConfigSource.default.loadIO[AppConfig]

      // Persistent4s
      database <- PostgresModule.make[IO, PrototypeEvent](
        PrototypeEvent.eventCodec,
        "postgres"
      )
      companies = CompanyRepository.make(database.sessions)
      companyService = CompanyServiceImpl(companies)(using
        database.commandRuntime
      )
      _ <- database.leaderElectedProjectionRuntime.startAll(registered =>
        registered.run(CompanyProjection(companies))
      )
      // In Memory Event Store
      eventStore <- InMemoryEventStore.create.toResource
      patientServiceImpl <- PatientServiceImpl(eventStore).toResource
      therapistServiceImpl <- TherapistServiceImpl(
        eventStore,
        companyService
      ).toResource
      appointmentServiceImpl <- AppointmentServiceImpl(
        eventStore,
        therapistServiceImpl,
        patientServiceImpl
      ).toResource
      dashboardServiceImpl <- DashboardServiceImpl(
        companyService,
        therapistServiceImpl,
        appointmentServiceImpl
      ).toResource
      seed <- Seed(
        appointmentServiceImpl,
        companyService,
        therapistServiceImpl,
        patientServiceImpl
      ).toResource
      _ <- seed.generateEvents().toResource
      tokens <- Resource.eval(Ref.of[IO, Map[Token, TherapistId]](Map.empty))
      authServiceImpl = new AuthServiceImpl(tokens, therapistServiceImpl)
      authChecker = AuthChecker(tokens)
      middleware = new AuthMiddleware(authChecker)
      routes <- Routes.all(
        greetingRoute,
        middleware,
        companyService,
        therapistServiceImpl,
        appointmentServiceImpl,
        patientServiceImpl,
        dashboardServiceImpl,
        authServiceImpl
      )
      server <- EmberServerBuilder
        .default[IO]
        .withHost(config.ember.host)
        .withPort(config.ember.port)
        .withHttpApp(ErrorLogging(routes.orNotFound))
        .build
    yield server

  def run: IO[Unit] = program.useForever
