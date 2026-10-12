package prototype.server.eventStore

import cats.effect.*

/** An in-memory implementation of an event store. It is not persistent and is
  * only useful for testing or prototyping.
  */
final class InMemoryEventStore[E] private (
    allEvents: Ref[IO, Map[String, List[E]]]
) extends EventStore[E]:

  override def load(tag: String): IO[List[E]] =
    allEvents.get.map(
      _.getOrElse(tag, List.empty)
    )

  def append(tag: String, events: List[E]): IO[Unit] =
    allEvents.update(all =>
      all.updated(tag, all.getOrElse(tag, List.empty) ++ events)
    )

object InMemoryEventStore:
  def create[E]: IO[InMemoryEventStore[E]] =
    Ref.of[IO, Map[String, List[E]]](Map.empty).map(new InMemoryEventStore(_))
