package prototype.server.eventStore

import cats.effect.*

/** An in-memory implementation of an event store. It is not persistent and is
  * only useful for testing or prototyping.
  */
final class InMemoryEventStore private (
    allEvents: Ref[IO, Map[String, List[Event]]]
) extends EventStore:

  override def load(tag: String): IO[List[Event]] =
    allEvents.get.map(
      _.getOrElse(tag, List.empty)
    )

  def append(tag: String, events: List[Event]): IO[Unit] =
    allEvents.update(all =>
      all.updated(tag, all.getOrElse(tag, List.empty) ++ events)
    )

object InMemoryEventStore:
  def create: IO[InMemoryEventStore] =
    Ref
      .of[IO, Map[String, List[Event]]](Map.empty)
      .map(ref => new InMemoryEventStore(ref))
