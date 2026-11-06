package prototype.client.live

object RefreshModel:
  type Position = Long

  enum Input[+K, +A]:
    case Opened
    case EventArrived(position: Position)
    case Loaded(key: K, value: A, position: Position)
    case Failed(key: K, error: Throwable)
    case KeyChanged(key: K)

  enum Phase:
    case Idle
    case InFlight(pending: Option[Position])

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

  def step[K, A](
      state: State[K, A],
      input: Input[K, A],
      invalidatePrevious: (K, K) => Boolean
  ): (State[K, A], Option[K]) =
    import Input.*
    input match
      case Opened =>
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
          case Phase.InFlight(pending) =>
            (
              state.copy(phase =
                Phase.InFlight(Some(pending.getOrElse(state.position)))
              ),
              None
            )

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

      case Loaded(key, _, _) if !state.key.contains(key) => (state, None)

      case Loaded(key, value, position) =>
        // An answer older than the value on screen does not replace it, but
        // still ends the load
        val answered =
          if position >= state.position then
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

      case Failed(key, _) if !state.key.contains(key) => (state, None)

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
