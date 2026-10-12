package prototype.server.eventStore

import cats.effect.*

trait Event

trait Command

trait EventStore:
  // Loads all events of a tag/aggregate. The events are ordered by their sequence number.
  def load(tag: String): IO[List[Event]]
  // Appends events to a tag/aggregate. The events are appended in order.
  def append(tag: String, events: List[Event]): IO[Unit]
