package prototype.server.support

import cats.effect.{IO, Resource}
import cats.effect.unsafe.IORuntime

import persistent4s.postgres.PostgresConfig

import org.scalatest.{AsyncTestSuite, BeforeAndAfterAll}

/** One Postgres container for the whole suite, started before the first test
  * and removed after the last. Each test asks for `freshDatabase`: its own
  * empty copy of the database, so tests never see each other's data.
  */
trait PostgresSuite extends AsyncTestSuite with BeforeAndAfterAll:

  private var container: Option[(PostgresConfig, IO[Unit])] = None

  override def beforeAll(): Unit =
    super.beforeAll()
    container = Some(TestPostgres.make.allocated.unsafeRunSync()(using IORuntime.global))

  override def afterAll(): Unit =
    try container.foreach((_, stop) => stop.unsafeRunSync()(using IORuntime.global))
    finally super.afterAll()

  def freshDatabase: Resource[IO, PostgresConfig] =
    container match
      case Some((postgres, _)) => TestPostgres.freshDatabase(postgres)
      case None                => Resource.raiseError[IO, PostgresConfig, Throwable](IllegalStateException("The suite's container is not started"))
