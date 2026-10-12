package prototype.server.eventStore

/** The business logic of one kind of stream, as two pure functions: `decide`
  * turns a command into events (or a rejection) given the current state, and
  * `evolve` applies one event to a state.
  */
trait Decider[S, C, E, R]:
  def initialState: S
  def decide(state: S, command: C): Either[R, List[E]]
  def evolve(state: S, event: E): S
