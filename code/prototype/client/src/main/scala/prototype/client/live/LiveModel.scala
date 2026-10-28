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

  final case class State[K, A, E](
      value: Option[A],
      key: Option[K],
      position: Position,
      tail: Vector[(Position, E)],
      loading: Boolean,
      error: Option[Throwable]
  )

  def initial[K, A, E]: State[K, A, E] =
    State(
      value = None,
      key = None,
      position = 0L,
      tail = Vector.empty,
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
        val newTail =
          (state.tail :+ (position -> event)).takeRight(MaxTailSize)
        (state.key, state.value) match
          case (Some(key), Some(value)) if position > state.position =>
            val newValue = evolve(key, value, event)
            state.copy(
              value = Some(newValue),
              position = position,
              tail = newTail
            )
          case _ => state.copy(tail = newTail)

      case Started(key) =>
        if state.key.contains(key) then state.copy(loading = true, error = None)
        else
          state.copy(
            value = None,
            key = Some(key),
            loading = true,
            error = None
          )

      case Loaded(key, value, position) =>
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
          loading = false,
          error = None
        )
      case Failed(_, error) =>
        state.copy(loading = false, error = Some(error))
end LiveModel
