package prototype.server

import scala.concurrent.duration.*

import cats.effect.*

import fs2.concurrent.Topic

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
import prototype.server.demo.{DemoDataGenerator, DemoDataServiceImpl}
import prototype.server.eventfeed.{
  EventFeed,
  EventFeedProjection,
  EventFeedRoutes,
  EventStoreServiceImpl
}

import persistent4s.EventEnvelope
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
      eventStore: EventStoreService[IO],
      eventFeed: EventFeed,
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
        // The live event streams never end on their own: without this, a
        // stop waits 30 s for them before releasing the port
        .withShutdownTimeout(1.second)
        .build
    yield server

  def makeWithConfig(
      config: AppConfig,
      withSeed: Boolean = false,
      withMonitoring: Boolean = false
  ): Resource[IO, Services] =
    for
      // Persistent4s
      database <- PostgresModule.makeWithConfig[IO](
        config.postgres
      )
      events <- database.domain(PrototypeEvent.eventCodec)
      // Its port is fixed: off in the tests, so they can run while a dev
      // server is up
      _ <-
        if withMonitoring then
          MonitoringServer
            .make(database.checkpoint, database.sendNotification, port"9595")
        else Resource.unit[IO]
      companies = CompanyRepository.make(database.sessions)
      companyService = CompanyServiceImpl(companies)(using
        events.commandRuntime
      )
      patients = PatientRepository.make(database.sessions)
      patientServiceImpl = PatientServiceImpl(patients)(using
        events.commandRuntime
      )
      therapists = TherapistRepository.make(database.sessions)
      therapistServiceImpl = TherapistServiceImpl(therapists)(using
        events.commandRuntime
      )
      appointments = AppointmentRepository.make(database.sessions)
      // The appointment commands wait for their projection: it publishes on
      // this topic after each commit (SyncCommandHandler)
      appointmentUpdates <- AppointmentProjection.updates.toResource
      appointmentServiceImpl = AppointmentServiceImpl(
        appointments,
        appointmentUpdates,
        therapistServiceImpl
      )(using events.commandRuntime)
      // Every stored event, published once for the open event streams
      publishedEvents <- Topic[IO, EventEnvelope[PrototypeEvent]].toResource
      _ <- events.leaderElectedProjectionRuntime.startAll { registered =>
        registered.run(CompanyProjection(companies))
        registered.run(TherapistProjection(therapists))
        registered.run(PatientProjection(patients))
        registered.run(
          AppointmentProjection(appointments),
          Some(appointmentUpdates)
        )
        registered.run(EventFeedProjection(publishedEvents))
      }
      dashboardServiceImpl <- DashboardServiceImpl(
        companyService,
        therapistServiceImpl,
        appointmentServiceImpl
      ).toResource
      // Only the dev account is created at boot; the demo data is generated on
      // request
      _ <-
        if withSeed then
          DevAccount
            .ensure(therapistServiceImpl)(using events.commandRuntime)
            .toResource
        else Resource.unit[IO]
      // Generates the demo data on request, through the command handlers;
      // the repositories are only read, to wait for the projections
      demoDataServiceImpl <- DemoDataServiceImpl
        .make(
          DemoDataGenerator(companies, therapists, patients, appointments)(using
            events.commandRuntime
          ),
          therapistServiceImpl
        )
        .toResource
      // Reads the stored events themselves, not a read model
      eventStoreServiceImpl = EventStoreServiceImpl(
        events.eventStore,
        PrototypeEvent.eventCodec
      )
      eventFeed = EventFeed(PrototypeEvent.eventCodec, publishedEvents)
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
        demoDataServiceImpl,
        eventStoreServiceImpl,
        EventFeedRoutes(eventFeed),
        authServiceImpl
      )
    yield Services(
      companyService,
      therapistServiceImpl,
      patientServiceImpl,
      appointmentServiceImpl,
      eventStoreServiceImpl,
      eventFeed,
      routes
    )

  def run: IO[Unit] = program.useForever
