package prototype.server.eventfeed

import scala.concurrent.duration.*

import cats.effect.{Fiber, IO}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.App
import prototype.server.support.{PostgresSuite, TestApp}

import org.http4s.{Method, Request, Uri}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import smithy4s.Blob
import smithy4s.json.Json

/** The live stream of new events, on a real Postgres in Docker: the event
  * feed projection publishes each event, the stream sends those published after
  * it opened. Each company created writes exactly one event, and on a fresh
  * database the first one is at position 1. Docker must be running.
  */
class EventFeedSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val address = Address("Example Street", "1", "Exampleville", "VD")

  private def createCompany(app: App.Services, name: String): IO[Unit] =
    app.companies.createCompany(name, address).void

  // Started in the background; the pause lets the stream subscribe before the
  // test writes the events it expects
  private def opened[A](read: IO[A]): IO[Fiber[IO, Throwable, A]] =
    read.timeout(10.seconds).start <* IO.sleep(500.millis)

  private def firstEvents(app: App.Services, count: Int): IO[List[EventMessage]] =
    app.eventFeed.stream
      .collect { case Some(message) => message }
      .take(count.toLong)
      .compile
      .toList

  private val streamRequest: Request[IO] =
    Request[IO](Method.GET, Uri.unsafeFromString("/api/events/stream"))

  // The first event of the response after the StreamOpened message: all its
  // lines (an event ends at a blank line; http4s writes `data:` before `id:`).
  // Keep-alive comments are skipped
  private def firstMessage(app: App.Services, request: Request[IO]): IO[List[String]] =
    app.routes.orNotFound
      .run(request)
      .flatMap(
        _.body
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .split(_.isEmpty)
          .map(_.toList.filterNot(_.startsWith(":")))
          .filter(_.nonEmpty)
          .drop(1)
          .take(1)
          .compile
          .lastOrError
      )

  "The event feed" should "deliver an event written after the stream opens" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          stream <- opened(firstEvents(app, count = 1))
          _ <- createCompany(app, "Clinic A")
          events <- stream.joinWithNever
        yield
          events.map(_.eventType) shouldBe List("CompanyCreated")
          events.head.payload should include("Clinic A")
      }
    }
  }

  it should "not send the events written before it opened" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          _ <- createCompany(app, "Clinic A")
          _ <- createCompany(app, "Clinic B")
          // Lets the projection publish them before the stream opens
          _ <- IO.sleep(1.second)
          stream <- opened(firstEvents(app, count = 1))
          _ <- createCompany(app, "Clinic C")
          events <- stream.joinWithNever
        yield events.map(_.cursor.position) shouldBe List(3L)
      }
    }
  }

  it should "send the event's cursor, with no SSE id" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          message <- opened(firstMessage(app, streamRequest))
          _ <- createCompany(app, "Clinic A")
          lines <- message.joinWithNever
          data = lines.collect {
            case line if line.startsWith("data: ") => line.stripPrefix("data: ")
          }
          message <- IO.fromEither(Json.read[EventMessage](Blob(data.mkString)))
        yield
          lines.filter(_.startsWith("id")) shouldBe empty
          message.cursor.position shouldBe 1L
          message.eventType shouldBe "CompanyCreated"
      }
    }
  }
end EventFeedSpec
