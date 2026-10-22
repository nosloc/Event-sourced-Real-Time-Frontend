package prototype.server

import cats.effect.*

import prototype.Greeting
import prototype.api.*
import prototype.server.auth.*
import prototype.server.config.AppConfig
import prototype.server.config.syntax.loadIO
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.api.AppointmentServiceImpl
import prototype.server.core.appointment.persistence.{
  AppointmentProjection,
  AppointmentRepository
}
import prototype.server.core.company.api.CompanyServiceImpl
import prototype.server.core.company.persistence.{
  CompanyProjection,
  CompanyRepository
}
import prototype.server.core.dashboards.*
import prototype.server.core.patient.api.PatientServiceImpl
import prototype.server.core.patient.persistence.{
  PatientProjection,
  PatientRepository
}
import prototype.server.core.therapist.api.TherapistServiceImpl
import prototype.server.core.therapist.persistence.{
  TherapistProjection,
  TherapistRepository
}

import persistent4s.monitoring.MonitoringServer
import persistent4s.postgres.PostgresModule

import com.comcast.ip4s.*
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

  final case class Services(
      companies: CompanyService[IO],
      therapists: TherapistService[IO],
      patients: PatientService[IO],
      appointments: AppointmentService[IO],
      routes: HttpRoutes[IO]
  )

  private val program =
    for
      config <- ConfigSource.default.loadIO[AppConfig]
      services <- makeWithConfig(config, withSeed = true, withMonitoring = true)
      server <- EmberServerBuilder
        .default[IO]
        .withHost(config.ember.host)
        .withPort(config.ember.port)
        .withHttpApp(ErrorLogging(services.routes.orNotFound))
        .build
    yield server

  def makeWithConfig(
      config: AppConfig,
      withSeed: Boolean = false,
      withMonitoring: Boolean = false
  ): Resource[IO, Services] =
    for
      // Persistent4s
      database <- PostgresModule.makeWithConfig[IO, PrototypeEvent](
        config.postgres,
        PrototypeEvent.eventCodec
      )
      // Its port is fixed: off in the tests, so they can run while a dev
      // server is up
      _ <-
        if withMonitoring then
          MonitoringServer
            .make(database.checkpoint, database.sendNotification, port"9595")
        else Resource.unit[IO]
      companies = CompanyRepository.make(database.sessions)
      companyService = CompanyServiceImpl(companies)(using
        database.commandRuntime
      )
      patients = PatientRepository.make(database.sessions)
      patientServiceImpl = PatientServiceImpl(patients)(using
        database.commandRuntime
      )
      therapists = TherapistRepository.make(database.sessions)
      therapistServiceImpl = TherapistServiceImpl(therapists)(using
        database.commandRuntime
      )
      appointments = AppointmentRepository.make(database.sessions)
      // The appointment commands wait for their projection: it publishes on
      // this topic after each commit (SyncCommandHandler)
      appointmentUpdates <- AppointmentProjection.updates.toResource
      appointmentServiceImpl = AppointmentServiceImpl(
        appointments,
        appointmentUpdates,
        therapistServiceImpl
      )(using database.commandRuntime)
      _ <- database.leaderElectedProjectionRuntime.startAll { registered =>
        registered.run(CompanyProjection(companies))
        registered.run(TherapistProjection(therapists))
        registered.run(PatientProjection(patients))
        registered.run(
          AppointmentProjection(appointments),
          Some(appointmentUpdates)
        )
      }
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
      _ <-
        if withSeed then seed.generateEvents().toResource
        else Resource.unit[IO]
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
    yield Services(
      companyService,
      therapistServiceImpl,
      patientServiceImpl,
      appointmentServiceImpl,
      routes
    )

  def run: IO[Unit] = program.useForever
