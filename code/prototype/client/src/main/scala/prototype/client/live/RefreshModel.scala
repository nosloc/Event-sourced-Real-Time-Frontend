package prototype.client.live

import scala.math.Ordering.Implicits.infixOrderingOps

import prototype.api.Cursor
import prototype.shared.Cursors
import prototype.shared.Cursors.given

object RefreshModel:

  enum Input[+K, +A]:
    case Opened(key: K)
    case EventArrived(cursor: Cursor)
    case Loaded(key: K, value: A, cursor: Cursor)
    case Failed(key: K, error: Throwable)
    case KeyChanged(key: K)

  enum Phase:
    // Nothing is happening
    case Idle
    // Waiting for an answer, `pending` is the highest cursor seen while the load is happening
    case InFlight(pending: Option[Cursor])

  /** What a step of the model decided: a load to launch, an event kept for the
    * next load, or the answer of a load. `RefreshedLiveData` runs the loads,
    * and records every outcome in the trace. Loads are numbered per live data.
    *
    * K is the type of the key, A is the type of the value.
    */
  enum Outcome[+K, +A]:
    // A load from scratch for `key` (stream opened, key changed): the loads
    // still in flight are dropped
    case InitialLoad(key: K, loadNumber: Int)
    // A load launched at once for the event at `cursor`
    case Load(key: K, cursor: Cursor, loadNumber: Int)
    // `servedBy` is the load that will cover the event
    case Batched(cursor: Cursor, servedBy: Int)
    // `value` is what is on screen after the answer
    case Answer(cursor: Cursor, loadNumber: Int, value: A)
    // An answer, and the load owed for the events batched meanwhile
    case AnswerAndLaunchPending(answer: Answer[K, A], load: Load[K, A])
    // The load failed
    case Error(loadNumber: Int)
    // A failure, and the load owed for the events batched meanwhile
    case ErrorAndLaunchPending(error: Error[K, A], load: Load[K, A])

  /** The refresh model
    *
    * @param key
    *   the actual key (None at the start)
    * @param value
    *   the value displayed
    * @param loading
    *   true if a new value is being loaded
    * @param error
    *   the error that occurred
    * @param cursor
    *   the cursor of the currently displayed value
    * @param phase
    *   which phase the model is in
    */
  final case class State[K, A](
      key: Option[K],
      value: Option[A],
      loading: Boolean,
      error: Option[Throwable],
      cursor: Cursor,
      phase: Phase,
      loadNumber: Int
  )

  def initial[K, A]: State[K, A] =
    State(
      key = None,
      value = None,
      loading = false,
      error = None,
      cursor = Cursors.Start,
      phase = Phase.Idle,
      loadNumber = 0
    )

  /** How the model evolves based on the current state and the input
    *
    * @param invalidatePrevious
    *   on a key change, whether the value gets invalidated
    * @param state
    *   the current state of the model
    * @param input
    *   the input that triggered the evolve
    *
    * @return
    *   the evolved state and what the step did, or None if nothing worth
    *   telling
    */
  def step[K, A](invalidatePrevious: (K, K) => Boolean)(
      state: State[K, A],
      input: Input[K, A]
  ): (State[K, A], Option[Outcome[K, A]]) =
    import Input.*
    val newLoadNumber = state.loadNumber + 1
    input match
      // The stream opened on a key the model does not have yet (the first
      // open): the same as a key change
      case Opened(key) if !state.key.contains(key) =>
        step(invalidatePrevious)(state, KeyChanged(key))

      // The reconnect case, trigger a reload if in Idle, or store it as pending
      case Opened(key) =>
        state.phase match
          case Phase.Idle =>
            (
              state.copy(
                loading = true,
                error = None,
                phase = Phase.InFlight(None),
                loadNumber = newLoadNumber
              ),
              Some(Outcome.InitialLoad(key, newLoadNumber))
            )
          // No event arrived: a reload is owed, but nothing was batched
          case Phase.InFlight(pending) =>
            val newPending = pending.getOrElse(state.cursor)
            (state.copy(phase = Phase.InFlight(Some(newPending))), None)

      // When an event arrives, trigger a refresh or store it as pending
      case EventArrived(cursor) =>
        state.phase match
          case Phase.Idle =>
            state.key match
              case Some(k) =>
                (
                  state.copy(
                    loading = true,
                    error = None,
                    phase = Phase.InFlight(None),
                    loadNumber = newLoadNumber
                  ),
                  Some(Outcome.Load(k, cursor, newLoadNumber))
                )
              case None => (state, None)
          case Phase.InFlight(pendingOpt) =>
            val newPending = pendingOpt match
              case Some(p) => Cursors.ordering.max(p, cursor)
              case None    => cursor
            (
              state.copy(phase = Phase.InFlight(Some(newPending))),
              Some(Outcome.Batched(cursor, newLoadNumber))
            )

      // Ignore a result that doesn't match the current key
      case Loaded(key, _, _) if !state.key.contains(key) =>
        (state, None)

      // Ignore the result if in Idle, take the value otherwise.
      // Trigger the pending load if any
      case Loaded(key, value, cursor) =>
        // An answer older than the value on screen does not replace it, but
        // still ends the load
        val answered =
          if state.value.isEmpty || cursor >= state.cursor then
            state.copy(value = Some(value), error = None, cursor = cursor)
          else state.copy(error = None)
        // What is on screen after this answer: the new value, or the one kept
        // when the answer is older than it
        val shown = answered.value.getOrElse(value)
        state.phase match
          case Phase.Idle           => (state, None)
          case Phase.InFlight(None) =>
            (
              answered.copy(loading = false, phase = Phase.Idle),
              Some(
                Outcome.Answer(cursor, state.loadNumber, shown)
              )
            )
          case Phase.InFlight(Some(newCursor)) =>
            (
              answered.copy(
                loading = true,
                phase = Phase.InFlight(None),
                loadNumber = newLoadNumber
              ),
              Some(
                Outcome.AnswerAndLaunchPending(
                  Outcome.Answer(cursor, state.loadNumber, shown),
                  Outcome.Load(key, newCursor, newLoadNumber)
                )
              )
            )

      // Ignore failure if the key doesn't match the current one
      case Failed(key, _) if !state.key.contains(key) => (state, None)

      // Ignore the failure if in Idle,
      // Otherwise store the error and launch the pending refresh if any
      case Failed(key, error) =>
        state.phase match
          case Phase.Idle           => (state, None)
          case Phase.InFlight(None) =>
            (
              state.copy(
                loading = false,
                error = Some(error),
                phase = Phase.Idle
              ),
              Some(Outcome.Error(state.loadNumber))
            )
          case Phase.InFlight(Some(newPending)) =>
            (
              state.copy(
                loading = true,
                error = Some(error),
                phase = Phase.InFlight(None),
                loadNumber = newLoadNumber
              ),
              Some(
                Outcome.ErrorAndLaunchPending(
                  Outcome.Error(state.loadNumber),
                  Outcome.Load(key, newPending, newLoadNumber)
                )
              )
            )
      // Launch a reload, and invalidate previous state if needed
      case KeyChanged(key) =>
        val newState =
          state.copy(
            key = Some(key),
            loading = true,
            error = None,
            phase = Phase.InFlight(None),
            loadNumber = newLoadNumber
          )
        (
          if state.key.exists(invalidatePrevious(_, key)) then
            newState.copy(value = None)
          else newState,
          Some(Outcome.InitialLoad(key, newLoadNumber))
        )
  end step
end RefreshModel
