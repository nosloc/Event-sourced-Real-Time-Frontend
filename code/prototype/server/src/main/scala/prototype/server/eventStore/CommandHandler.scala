package prototype.server.eventStore

import cats.effect.*

/** A command handler is the event-sourced layer that handles commands and
  * communicates with the event store.
  *
  * @param decider
  *   The decider contains the business logic of the validation of commands and
  *   the sequence of events that they produce.
  * @param store
  *   The event store is the persistence layer that stores the events.
  * @param viewer
  *   The viewer is in charge of holding the current state of an object and
  *   evolving it with events.
  */
final class CommandHandler[S, C, E, R](
    decider: Decider[S, C, E, R],
    viewer: Viewer[S, E],
    store: EventStore[E]
):

  /** The current state of an object, based on the view */
  def state(tag: String): IO[S] =
    viewer.getState(tag)

  /** Runs the command. A rejection is an expected outcome (`Left`). */
  def handle(tag: String, command: C): IO[Either[R, S]] =
    for
      // Load the event, reconstruct the state and validate the command
      eventsOrRejection <- store.load(tag).map { events =>
        val current = fold(decider.initialState, events)
        decider.decide(current, command)
      }
      // If the command is valid, append the events and update the view
      newState <- eventsOrRejection match
        case Left(rejection) => IO.pure(Left(rejection))
        case Right(events)   =>
          for
            _ <- store.append(tag, events)
            state <- viewer.updateState(tag, events)
          yield Right(state)
    yield newState

  private def fold(from: S, events: Iterable[E]): S =
    events.foldLeft(from)(decider.evolve)
