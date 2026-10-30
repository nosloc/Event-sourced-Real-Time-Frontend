package prototype.server.support

import cats.effect.{IO, Resource}

import persistent4s.postgres.{PostgresConfig, PostgresModule}

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.{Meter, MeterProvider}
import org.typelevel.otel4s.trace.{Tracer, TracerProvider}
import skunk.Session

/** The connection pool of the app's Postgres module, for the specs that test a
  * repository on its own. Opening it also creates the event store's tables.
  */
object TestDatabase:

  private given Logger[IO] = Slf4jLogger.getLogger[IO]
  private given Tracer[IO] = Tracer.Implicits.noop
  private given Meter[IO] = Meter.Implicits.noop
  private given TracerProvider[IO] = TracerProvider.noop[IO]
  private given MeterProvider[IO] = MeterProvider.noop[IO]

  def sessions(
      postgres: PostgresConfig
  ): Resource[IO, Resource[IO, Session[IO]]] =
    PostgresModule
      .makeWithConfig[IO](postgres)
      .map(_.sessions)
