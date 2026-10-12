package prototype.server.eventStore

import cats.effect.*

/** A viewer is a read-only projection of a stream of events.
  */
trait Viewer[S, E]:
  def evolve(state: S, event: E): S
  def getState(tag: String): IO[S]
  def updateState(tag: String, events: List[E]): IO[S]
