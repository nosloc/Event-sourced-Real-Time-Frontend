package prototype.server.eventfeed

import scala.concurrent.duration.*

import cats.effect.IO

import fs2.Stream

import org.http4s.{HttpRoutes, ServerSentEvent}
import org.http4s.dsl.io.*
import org.http4s.headers.`Last-Event-Id`
import smithy4s.json.Json
import prototype.StreamOpened

/** `GET /api/events/stream`: the new events as server-sent events, each with
  * its position as id. A browser that reconnects sends back the last one in
  * `Last-Event-ID`, and only the events above it are sent. The first message, a
  * `StreamOpened` event, says the stream has subscribed: the client can load
  * the current state from then on. Not authenticated for now: `EventSource`
  * cannot send the bearer token.
  */
object EventFeedRoutes:

  /** Often enough that proxies do not close a quiet connection */
  val KeepAliveEvery: FiniteDuration = 15.seconds

  def apply(feed: EventFeed): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case request @ GET -> Root / "api" / "events" / "stream" =>
        val start = request.headers
          .get[`Last-Event-Id`]
          .map(_.id.value)
        start match
          // Positions start at 1
          case None       => Ok(messages(feed, 0L))
          case Some(text) =>
            text.toLongOption match
              case None           => BadRequest(s"Not an event position: $text")
              case Some(position) => Ok(messages(feed, position))
    }

  private def messages(
      feed: EventFeed,
      after: Long
  ): Stream[IO, ServerSentEvent] =
    val events = feed
      .stream(after)
      .map {
        // No id, so a reconnect still resumes from the last event
        case None =>
          ServerSentEvent(
            eventType = Some(StreamOpened.EventType),
            data = Some(StreamOpened.data(after))
          )
        case Some(view) =>
          ServerSentEvent(
            data = Some(Json.writeBlob(view).toUTF8String),
            id = Some(ServerSentEvent.EventId(view.position.toString))
          )
      }
    val keepAlive = Stream
      .awakeEvery[IO](KeepAliveEvery)
      .as(ServerSentEvent(comment = Some("keep-alive")))
    events.merge(keepAlive)
end EventFeedRoutes
