package prototype.server.eventfeed

import cats.effect.IO

import fs2.Stream
import fs2.concurrent.Topic

import prototype.api.EventView
import prototype.server.core.PrototypeEvent

import persistent4s.{EventCodec, EventEnvelope}

/** The new events, as the `EventFeedProjection` publishes them. The past events
  * are not sent: the browser asks for them through the event store service.
  */
final class EventFeed(
    codec: EventCodec[PrototypeEvent],
    published: Topic[IO, EventEnvelope[PrototypeEvent]]
):

  /** Never ends on its own. The subscription is in place before the first
    * event is sent, and the events whose position is not above `after` are
    * left out.
    */
  def stream(after: Long): Stream[IO, EventView] =
    Stream
      .resource(published.subscribeAwait(EventFeed.QueueSize))
      .flatten
      .filter(_.metadata.globalPosition > after)
      .evalMap(envelope => IO.fromEither(EventViews.from(envelope, codec)))
end EventFeed

object EventFeed:

  /** New events one stream may have waiting before the projection waits for it */
  val QueueSize: Int = 512
