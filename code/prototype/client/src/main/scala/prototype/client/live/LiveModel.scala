package prototype.client.live

object LiveModel:
  type Position = Long

  /** What can change the state of a live value: an event received, or a load of
    * its snapshot that starts, succeeds or fails.
    *
    * K is the type of the key, A is the type of the value, E is the type of the
    * event
    */
  enum Input[+K, +A, +E]:
    // The client received an event from the server
    case Received(position: Position, event: E)
    // A load of the snapshot for this key has started
    case Started(key: K)
    // The snapshot for this key arrived, with the position it includes
    case Loaded(key: K, value: A, position: Position)
    // The load of the snapshot failed
    case Failed(key: K, error: Throwable)

  /** `highestDiscardedPosition` is the highest position ever dropped from the
    * tail, `refetches` counts the snapshots of the current key rejected in a
    * row for that reason.
    */
  final case class State[K, A, E](
      value: Option[A],
      key: Option[K],
      position: Position,
      tail: Vector[(Position, E)],
      highestDiscardedPosition: Option[Position],
      refetches: Int,
      loading: Boolean,
      error: Option[Throwable]
  ):
    def applyIncomingEvent = refetches == 0

  def initial[K, A, E]: State[K, A, E] =
    State(
      value = None,
      key = None,
      position = 0L,
      tail = Vector.empty,
      highestDiscardedPosition = None,
      refetches = 0,
      loading = false,
      error = None
    )

  val MaxTailSize = 256

  def step[K, A, E](
      evolve: (K, A, E) => A
  )(state: State[K, A, E], input: Input[K, A, E]): State[K, A, E] =
    import Input.*
    input match
      case Received(position, event) =>
        // The most recent events, in the order received
        val (discarded, newTail) =
          (state.tail :+ (position -> event)).splitAt(
            state.tail.size + 1 - MaxTailSize
          )
        val newHighestDiscardedPosition =
          (state.highestDiscardedPosition ++ discarded.map(_._1)).maxOption

        (state.key, state.value) match
          case (Some(key), Some(value))
              if position > state.position && state.applyIncomingEvent =>
            val newValue = evolve(key, value, event)
            state.copy(
              value = Some(newValue),
              position = position,
              highestDiscardedPosition = newHighestDiscardedPosition,
              tail = newTail
            )
          case _ =>
            state.copy(
              tail = newTail,
              highestDiscardedPosition = newHighestDiscardedPosition
            )

      case Started(key) =>
        if state.key.contains(key) then state.copy(loading = true, error = None)
        else
          // A new key: nothing shown yet, and the old position was the old key's
          state.copy(
            value = None,
            key = Some(key),
            position = 0L,
            refetches = 0,
            loading = true,
            error = None
          )

      case Loaded(key, value, position) =>
        if state.highestDiscardedPosition.exists(_ > position) then
          // Events after the snapshot were dropped, so it cannot be completed:
          // show it only if newer, freeze the value and ask for a new load
          val (newValue, newPosition) =
            if state.position < position then (Some(value), position)
            else (state.value, state.position)
          state.copy(
            refetches = state.refetches + 1,
            loading = true,
            value = newValue,
            position = newPosition
          )
        else
          val eventAfterPosition =
            state.tail.filter(_._1 > position)
          val newPosition =
            eventAfterPosition.map(_._1).maxOption.getOrElse(position)
          val newValue =
            eventAfterPosition.map(_._2).foldLeft(value) { case (v, e) =>
              evolve(key, v, e)
            }
          state.copy(
            value = Some(newValue),
            key = Some(key),
            position = newPosition,
            refetches = 0,
            loading = false,
            error = None
          )
      case Failed(_, error) =>
        state.copy(loading = false, error = Some(error))
end LiveModel
