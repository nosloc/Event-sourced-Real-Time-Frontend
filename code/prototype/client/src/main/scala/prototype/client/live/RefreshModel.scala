package prototype.client.live

object RefreshModel:
  type Position = Long

  enum Input[+K, +A]:
    case Opened(key: K)
    case EventArrived(position: Position)
    case Loaded(key: K, value: A, position: Position)
    case Failed(key: K, error: Throwable)
    case KeyChanged(key: K)

  enum Phase:
    case Idle
    case InFlight(pending: Option[Position])

  enum Outcome[+K, +A]:
    case InitialLoad(key: K, loadNumber: Int)
    case Load(key: K, position: Position, loadNumber: Int)
    // `servedBy` is the load that will cover the event
    case Batched(position: Position, servedBy: Int)
    // `value` is what is on screen after the answer
    case Answer(position: Position, loadNumber: Int, value: A)
    case AnswerAndLaunchPending(answer: Answer[K, A], load: Load[K, A])
    case Error(loadNumber: Int)
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
    * @param position
    *   the position of the currently displayed value
    * @param phase
    *   which phase the model is in
    */
  final case class State[K, A](
      key: Option[K],
      value: Option[A],
      loading: Boolean,
      error: Option[Throwable],
      position: Position,
      phase: Phase,
      loadNumber: Int
  )

  def initial[K, A]: State[K, A] =
    State(
      key = None,
      value = None,
      loading = false,
      error = None,
      position = 0L,
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
            val newPending = pending.getOrElse(state.position)
            (state.copy(phase = Phase.InFlight(Some(newPending))), None)

      // When an event arrives, trigger a refresh or store it as pending
      case EventArrived(position) =>
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
                  Some(Outcome.Load(k, position, newLoadNumber))
                )
              case None => (state, None)
          case Phase.InFlight(posOpt) =>
            val newPos = posOpt match
              case Some(p) => Math.max(p, position)
              case None    => position
            (
              state.copy(phase = Phase.InFlight(Some(newPos))),
              Some(Outcome.Batched(position, newLoadNumber))
            )

      // Ignore a result that doesn't match the current key
      case Loaded(key, _, _) if !state.key.contains(key) =>
        (state, None)

      // Ignore the result if in Idle, take the value otherwise.
      // Trigger the pending load if any
      case Loaded(key, value, position) =>
        // An answer older than the value on screen does not replace it, but
        // still ends the load
        val answered =
          if state.value.isEmpty || position >= state.position then
            state.copy(value = Some(value), error = None, position = position)
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
                Outcome.Answer(position, state.loadNumber, shown)
              )
            )
          case Phase.InFlight(Some(newPosition)) =>
            (
              answered.copy(
                loading = true,
                phase = Phase.InFlight(None),
                loadNumber = newLoadNumber
              ),
              Some(
                Outcome.AnswerAndLaunchPending(
                  Outcome.Answer(position, state.loadNumber, shown),
                  Outcome.Load(key, newPosition, newLoadNumber)
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
          case Phase.InFlight(Some(newPos)) =>
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
                  Outcome.Load(key, newPos, newLoadNumber)
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
