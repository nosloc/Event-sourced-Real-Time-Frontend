package prototype.server.support

import cats.effect.{IO, Resource}

import persistent4s.postgres.PostgresConfig

import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.utility.MountableFile

/** A throwaway Postgres for the tests that need a real database. It starts in
  * Docker on a free port with the read tables already created, and is removed
  * when the resource is released. Docker must be running.
  */
object TestPostgres:

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
          maxConnections = 4
        )
      )
