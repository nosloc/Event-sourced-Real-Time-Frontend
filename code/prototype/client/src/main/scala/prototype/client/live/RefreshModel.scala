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
    // Nothing is happening
    case Idle
    // Waiting for an answer, `pending` is the highest position seen while the load is happening
    case InFlight(pending: Option[Position])

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
      phase: Phase
  )

  def initial[K, A]: State[K, A] =
    State(
      key = None,
      value = None,
      loading = false,
      error = None,
      position = 0L,
      phase = Phase.Idle
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
    *   the evolved state and the key that need a reload, or None if no reload
    */
  def step[K, A](invalidatePrevious: (K, K) => Boolean)(
      state: State[K, A],
      input: Input[K, A]
  ): (State[K, A], Option[K]) =
    import Input.*
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
                phase = Phase.InFlight(None)
              ),
              Some(key)
            )
          case Phase.InFlight(pending) =>
            (
              state.copy(phase =
                Phase.InFlight(Some(pending.getOrElse(state.position)))
              ),
              None
            )

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
                    phase = Phase.InFlight(None)
                  ),
                  Some(k)
                )
              case None => (state, None)
          case Phase.InFlight(posOpt) =>
            val newPos = posOpt match
              case Some(p) => Math.max(p, position)
              case None    => position
            (state.copy(phase = Phase.InFlight(Some(newPos))), None)

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
        state.phase match
          case Phase.Idle           => (state, None)
          case Phase.InFlight(None) =>
            (answered.copy(loading = false, phase = Phase.Idle), None)
          case Phase.InFlight(Some(_)) =>
            (
              answered.copy(loading = true, phase = Phase.InFlight(None)),
              Some(key)
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
              None
            )
          case Phase.InFlight(Some(_)) =>
            state.key match
              case None =>
                (
                  state.copy(
                    loading = false,
                    error = Some(error),
                    phase = Phase.Idle
                  ),
                  None
                )
              case Some(k) =>
                (
                  state.copy(
                    loading = true,
                    error = Some(error),
                    phase = Phase.InFlight(None)
                  ),
                  Some(k)
                )
      // Launch a reload, and invalidate previous state if needed
      case KeyChanged(key) =>
        val newState =
          state.copy(
            key = Some(key),
            loading = true,
            error = None,
            phase = Phase.InFlight(None)
          )
        (
          if state.key.exists(invalidatePrevious(_, key)) then
            newState.copy(value = None)
          else newState,
          Some(key)
        )
  end step
end RefreshModel
