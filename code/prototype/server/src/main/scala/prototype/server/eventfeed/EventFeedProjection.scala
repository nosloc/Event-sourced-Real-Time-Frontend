package prototype.server.eventfeed

import cats.effect.IO

import fs2.concurrent.Topic

import prototype.server.core.PrototypeEvent

import persistent4s.{EventEnvelope, EventTypeName, StatelessProjection}

final class EventFeedProjection(
    published: Topic[IO, EventEnvelope[PrototypeEvent]]
) extends StatelessProjection[IO, PrototypeEvent]:

  override def name: String = "event-feed-projection"

  override def filter: Set[EventTypeName] = Set.empty

  // The topic only closes when the server stops: nothing is waiting for the
  // event then, so a closed topic is not an error
  override def handle(event: EventEnvelope[PrototypeEvent]): IO[Unit] =
    published.publish1(event).void
