package prototype.server.support

import cats.effect.{IO, Resource}
import cats.effect.std.Console

import persistent4s.postgres.PostgresConfig

import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.utility.MountableFile
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import skunk.Session
import skunk.syntax.all.*

/** A throwaway Postgres for the tests that need a real database. It starts in
  * Docker on a free port with the read tables already created, and is removed
  * when the resource is released. Docker must be running.
  *
  * Starting a container takes about a second, so a suite starts one (see
  * `PostgresSuite`) and every test works in its own copy of the database.
  */
object TestPostgres:

  private given TracerProvider[IO] = TracerProvider.noop[IO]
  private given MeterProvider[IO] = MeterProvider.noop[IO]
  private given Console[IO] = Console.make[IO]

  def make: Resource[IO, PostgresConfig] =
    Resource
      .make(IO.blocking {
        val container = new PostgreSQLContainer[Nothing]("postgres:16-alpine")
        // Postgres runs every .sql file in this folder on first start. The mode
        // 644 keeps the file readable by the container's own user.
        container.withCopyFileToContainer(
          MountableFile.forClasspathResource(
            "db/init-views.sql",
            Integer.parseInt("644", 8)
          ),
          "/docker-entrypoint-initdb.d/init-views.sql"
        )
        container.start()
        container
      })(container => IO.blocking(container.stop()))
      .map(container =>
        PostgresConfig(
          host = container.getHost,
          port = container.getMappedPort(5432),
          user = container.getUsername,
          password = container.getPassword,
          database = container.getDatabaseName,
          // Each projection keeps one connection for LISTEN: the pool must
          // have room for them and for the commands
          maxConnections = 16
        )
      )

  /** A new database copied from the container's one (the read tables, no data),
    * dropped when the resource is released. Nothing may connect to the
    * container's database itself, or Postgres refuses to copy it.
    */
  def freshDatabase(container: PostgresConfig): Resource[IO, PostgresConfig] =
    // Postgres's own maintenance database, to create and drop the others
    val admin = Session
      .Builder[IO]
      .withHost(container.host)
      .withPort(container.port)
      .withUserAndPassword(container.user, container.password)
      .withDatabase("postgres")
      .single
    for
      name <- Resource.eval(
        IO.randomUUID.map(id => "test_" + id.toString.replace("-", ""))
      )
      // A database name cannot be a bind parameter, so it is spliced as text;
      // it is generated just above, never taken from outside
      // scalafix:off DisableSyntax.skunkRawInterpolation
      _ <- Resource.make(
        admin.use(
          _.execute(
            sql"CREATE DATABASE #$name TEMPLATE #${container.database}".command
          )
        )
      )(_ =>
        // FORCE closes the connections a test left open
        admin
          .use(_.execute(sql"DROP DATABASE #$name WITH (FORCE)".command))
          .void
      )
    // scalafix:on DisableSyntax.skunkRawInterpolation
    yield container.copy(database = name)
