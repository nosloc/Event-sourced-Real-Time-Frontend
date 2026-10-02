package prototype.server

import cats.effect.{IO, IOApp}
import org.http4s.*
import org.http4s.dsl.io.*
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder

import prototype.Greeting

object Server extends IOApp.Simple:

  private val routes = HttpRoutes.of[IO] { case GET -> Root / "api" / "hello" =>
    Ok(Greeting.greet("World !"))
  }

  def run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(host"127.0.0.1")
      .withPort(port"9000")
      .withHttpApp(routes.orNotFound)
      .build
      .useForever
