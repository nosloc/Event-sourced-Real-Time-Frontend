package prototype.server.eventfeed

import cats.effect.IO

import fs2.Stream
import fs2.concurrent.Topic

import prototype.api.EventMessage
import prototype.shared.events.PrototypeEvent

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
    * new event, in the order the projection delivers them.
    */
  def stream: Stream[IO, Option[EventMessage]] =
    Stream
      .resource(published.subscribeAwait(EventFeed.QueueSize))
      .flatMap(events =>
        Stream.emit(None)
          ++ events
            .evalMap(envelope =>
              IO.fromEither(EventMessages.from(envelope, codec))
            )
            .map(Some(_))
      )
end EventFeed

object EventFeed:

  /** New events a stream may have waiting before the projection waits */
  val QueueSize: Int = 512
