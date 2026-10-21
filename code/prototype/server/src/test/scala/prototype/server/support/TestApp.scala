package prototype.server.support

import scala.concurrent.duration.*

import cats.effect.{IO, Resource}

import prototype.server.App
import prototype.server.config.{AppConfig, EmberConfig}

import persistent4s.postgres.PostgresConfig

import com.comcast.ip4s.*

object TestApp:

  /** The HTTP server is never started by the tests, so its settings only have
    * to be valid.
    */
  def config(postgres: PostgresConfig): AppConfig =
    AppConfig(postgres, EmberConfig(host"127.0.0.1", port"0", host"127.0.0.1"))

  /** The services on the given database, without generated data. Closing the
    * resource stops the projection and the connections, not the database.
    */
  def make(postgres: PostgresConfig): Resource[IO, App.Services] =
    App.makeWithConfig(config(postgres))

  /** Repeats `read` until `done` accepts its result, failing after ten seconds.
    * The read side is filled by the projection a moment after a command.
    */
  def eventually[A](read: IO[A])(done: A => Boolean): IO[A] =
    def loop: IO[A] =
      read.flatMap(value =>
        if done(value) then IO.pure(value) else IO.sleep(100.millis) *> loop
      )
    loop.timeout(10.seconds)
