package prototype.server.eventfeed

import scala.concurrent.duration.*

import cats.effect.IO

import fs2.Stream

import prototype.shared.StreamOpened

import org.http4s.{HttpRoutes, ServerSentEvent}
import org.http4s.dsl.io.*
import smithy4s.json.Json

/** `GET /api/events/stream`: the new events as server-sent events. The first
  * message, a `StreamOpened` event, says the stream has subscribed: the client
  * can load the current state from then on. The events carry no id, so a browser
  * that reconnects sends no `Last-Event-ID`: a new stream starts with the events
  * published after it, and the client reloads after `StreamOpened`. Not
  * authenticated for now: `EventSource` cannot send the bearer token.
  */
object EventFeedRoutes:

  /** Often enough that proxies do not close a quiet connection */
  val KeepAliveEvery: FiniteDuration = 15.seconds

  def apply(feed: EventFeed): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "api" / "events" / "stream" =>
        Ok(messages(feed))
    }

  private def messages(feed: EventFeed): Stream[IO, ServerSentEvent] =
    val events = feed.stream
      .map {
        case None =>
          ServerSentEvent(
            eventType = Some(StreamOpened.EventType),
            data = Some(StreamOpened.Data)
          )
        case Some(message) =>
          ServerSentEvent(data = Some(Json.writeBlob(message).toUTF8String))
      }
    val keepAlive = Stream
      .awakeEvery[IO](KeepAliveEvery)
      .as(ServerSentEvent(comment = Some("keep-alive")))
    events.merge(keepAlive)
end EventFeedRoutes
