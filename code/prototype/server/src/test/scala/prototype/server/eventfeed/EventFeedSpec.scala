package prototype.server.eventfeed

import scala.concurrent.duration.*

import cats.effect.{Fiber, IO}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.App
import prototype.server.support.{PostgresSuite, TestApp}

import org.http4s.{Headers, Method, Request, ServerSentEvent, Status, Uri}
import org.http4s.headers.`Last-Event-Id`
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import smithy4s.Blob
import smithy4s.json.Json

/** The live stream of new events, on a real Postgres in Docker: the event
  * feed projection publishes each event, the stream sends those above its
  * position. Each company created writes exactly one event, and on a fresh
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

  private def firstEvents(app: App.Services, after: Long, count: Int): IO[List[EventView]] =
    app.eventFeed
      .stream(after)
      .collect { case Some(view) => view }
      .take(count.toLong)
      .compile
      .toList

  private def streamRequest(headers: Headers = Headers.empty): Request[IO] =
    Request[IO](Method.GET, Uri.unsafeFromString("/api/events/stream"), headers = headers)

  // The first event of the response, after the StreamOpened message: its id
  // and its data lines
  private def firstMessage(app: App.Services, request: Request[IO]): IO[(String, String)] =
    app.routes.orNotFound
      .run(request)
      .flatMap(
        _.body
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
          .filter(line => line.startsWith("id: ") || line.startsWith("data: "))
          .drop(1)
          .take(2)
          .compile
          .toList
      )
      .map(lines =>
        val id = lines.collectFirst { case l if l.startsWith("id: ") => l.stripPrefix("id: ") }
        val data = lines.collectFirst { case l if l.startsWith("data: ") => l.stripPrefix("data: ") }
        (id.getOrElse(""), data.getOrElse(""))
      )

  private def lastEventId(id: String): Headers =
    Headers(`Last-Event-Id`(ServerSentEvent.EventId(id)))

  "The event feed" should "deliver an event written after the stream opens" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          stream <- opened(firstEvents(app, after = 0L, count = 1))
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
          stream <- opened(firstEvents(app, after = 0L, count = 1))
          _ <- createCompany(app, "Clinic C")
          events <- stream.joinWithNever
        yield events.map(_.position) shouldBe List(3L)
      }
    }
  }

  it should "leave out the events whose position is not above after" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          stream <- opened(firstEvents(app, after = 2L, count = 1))
          _ <- createCompany(app, "Clinic A")
          _ <- createCompany(app, "Clinic B")
          _ <- createCompany(app, "Clinic C")
          events <- stream.joinWithNever
        yield events.map(_.position) shouldBe List(3L)
      }
    }
  }

  it should "resume after the position in Last-Event-ID" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          message <- opened(firstMessage(app, streamRequest(lastEventId("2"))))
          _ <- createCompany(app, "Clinic A")
          _ <- createCompany(app, "Clinic B")
          _ <- createCompany(app, "Clinic C")
          (id, _) <- message.joinWithNever
        yield id shouldBe "3"
      }
    }
  }

  it should "refuse a position that is not a number with 400" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          response <- app.routes.orNotFound.run(streamRequest(lastEventId("12x")))
        yield response.status shouldBe Status.BadRequest
      }
    }
  }

  it should "use the event's position as the SSE id" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          message <- opened(firstMessage(app, streamRequest()))
          _ <- createCompany(app, "Clinic A")
          (id, data) <- message.joinWithNever
          view <- IO.fromEither(Json.read[EventView](Blob(data)))
        yield
          id shouldBe "1"
          view.position shouldBe 1L
          view.eventType shouldBe "CompanyCreated"
      }
    }
  }
end EventFeedSpec
