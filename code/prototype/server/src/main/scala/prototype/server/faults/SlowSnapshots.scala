package prototype.server.faults

import cats.data.Kleisli
import cats.effect.IO

import org.http4s.*
import org.http4s.dsl.io.*

/** Slows the snapshots down, first read the list, then wait for the snapshot
  * delay before sending it.
  */
object SlowSnapshots:

  def apply(faults: FaultInjection)(routes: HttpRoutes[IO]): HttpRoutes[IO] =
    Kleisli { request =>
      val response = routes(request)
      // After the route answered: the list is already read when the wait starts
      if isSnapshot(request) then
        response.semiflatTap(_ => faults.snapshotDelay.flatMap(IO.sleep))
      else response
    }

  // What requests are concerned
  def isSnapshot(request: Request[IO]): Boolean = request match
    case GET -> Root / "api" / ("companies" | "therapists" | "patients") =>
      true
    case GET -> Root / "api" / ("companies" | "therapists") / _ /
        "appointments" =>
      true
    case GET -> Root / "api" / "companies" / _ / "dashboards" / _ => true
    case _                                                        => false
