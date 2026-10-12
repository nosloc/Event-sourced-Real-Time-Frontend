package prototype.server.eventStore

import cats.effect.*

trait EventStore[E]:
  // Loads all events of a tag/aggregate. The events are ordered by their sequence number.
  def load(tag: String): IO[List[E]]
  // Appends events to a tag/aggregate. The events are appended in order.
  def append(tag: String, events: List[E]): IO[Unit]
