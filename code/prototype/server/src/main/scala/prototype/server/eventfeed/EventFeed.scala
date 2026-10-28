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

  /** Never ends on its own. Starts with a `None`, sent once the subscription is
    * in place, so no event published after it can be missed. Then one `Some` per
    * new event, leaving out those whose position is not above `after`.
    */
  def stream(after: Long): Stream[IO, Option[EventView]] =
    Stream
      .resource(published.subscribeAwait(EventFeed.QueueSize))
      .flatMap(events =>
        Stream.emit(None)
          ++ events
            .filter(_.metadata.globalPosition > after)
            .evalMap(envelope =>
              IO.fromEither(EventViews.from(envelope, codec))
            )
            .map(Some(_))
      )
end EventFeed

object EventFeed:

  /** New events a stream may have waiting before the projection waits */
  val QueueSize: Int = 512
