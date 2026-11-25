package prototype.client.live

import scala.math.Ordering.Implicits.infixOrderingOps

import prototype.api.Cursor
import prototype.shared.Cursors
import prototype.shared.Cursors.given

object LiveModel:

  /** What can change the state of a live value: an event received, the stream
    * closing, or a load of its snapshot that starts, succeeds or fails.
    *
    * K is the type of the key, A is the type of the value, E is the type of the
    * event
    */
  enum Input[+K, +A, +E]:
    // The client received an event from the server
    case Received(cursor: Cursor, event: E)
    // The stream closed: events are missed until it reopens
    case Closed
    // A load of the snapshot for this key has started. Assumed only while the
    // stream is open: `ReplayedLiveData` starts none while it is closed
    case Started(key: K)
    // The snapshot for this key arrived, with the cursor it includes
    case Loaded(key: K, value: A, cursor: Cursor)
    // The load of the snapshot failed
    case Failed(key: K, error: Throwable)

  /** `highestDiscarded` is the highest cursor ever dropped from the
    * tail, `refetches` counts the snapshots of the current key rejected in a
    * row for that reason.
    *
    * `behind`: the value may miss events (stream closed or load started, no
    * snapshot accepted since), so events only go to the tail.
    * `closedSinceStarted`: the stream closed during the current load, so its
    * snapshot may miss events and does not lift `behind`.
    */
  final case class State[K, A, E](
      value: Option[A],
      key: Option[K],
      cursor: Cursor,
      tail: Vector[(Cursor, E)],
      highestDiscarded: Option[Cursor],
      refetches: Int,
      behind: Boolean,
      closedSinceStarted: Boolean,
      loading: Boolean,
      error: Option[Throwable]
  ):
    def applyIncomingEvent = refetches == 0 && !behind

  def initial[K, A, E]: State[K, A, E] =
    State(
      value = None,
      key = None,
      cursor = Cursors.Start,
      tail = Vector.empty,
      highestDiscarded = None,
      refetches = 0,
      behind = false,
      closedSinceStarted = false,
      loading = false,
      error = None
    )

  val MaxTailSize = 256

  def step[K, A, E](
      evolve: (K, A, E) => A
  )(state: State[K, A, E], input: Input[K, A, E]): State[K, A, E] =
    import Input.*
    input match
      case Received(cursor, event) =>
        // The most recent events, in the order received
        val (discarded, newTail) =
          (state.tail :+ (cursor -> event)).splitAt(
            state.tail.size + 1 - MaxTailSize
          )
        val newHighestDiscarded =
          (state.highestDiscarded ++ discarded.map(_._1)).maxOption

        (state.key, state.value) match
          case (Some(key), Some(value))
              if cursor > state.cursor && state.applyIncomingEvent =>
            val newValue = evolve(key, value, event)
            state.copy(
              value = Some(newValue),
              cursor = cursor,
              highestDiscarded = newHighestDiscarded,
              tail = newTail
            )
          case _ =>
            state.copy(
              tail = newTail,
              highestDiscarded = newHighestDiscarded
            )

      case Closed =>
        state.copy(behind = true, closedSinceStarted = true)

      case Started(key) =>
        // A reload may follow missed events (a reconnect): behind until accepted
        if state.key.contains(key) then
          state.copy(
            behind = true,
            closedSinceStarted = false,
            loading = true,
            error = None
          )
        else
          // A new key: nothing shown yet, and the old cursor was the old key's
          state.copy(
            value = None,
            key = Some(key),
            cursor = Cursors.Start,
            refetches = 0,
            behind = true,
            closedSinceStarted = false,
            loading = true,
            error = None
          )

      case Loaded(key, value, cursor) =>
        if state.highestDiscarded.exists(_ > cursor) then
          // Events after the snapshot were dropped, so it cannot be completed:
          // show it only if newer, freeze the value and ask for a new load
          val (newValue, newCursor) =
            if state.cursor < cursor then (Some(value), cursor)
            else (state.value, state.cursor)
          state.copy(
            refetches = state.refetches + 1,
            loading = true,
            value = newValue,
            cursor = newCursor
          )
        else
          val eventsAfterCursor =
            state.tail.filter(_._1 > cursor)
          val newCursor =
            eventsAfterCursor.map(_._1).maxOption.getOrElse(cursor)
          val newValue =
            eventsAfterCursor.map(_._2).foldLeft(value) { case (v, e) =>
              evolve(key, v, e)
            }
          // Still behind if the stream closed during this load
          state.copy(
            value = Some(newValue),
            key = Some(key),
            cursor = newCursor,
            refetches = 0,
            behind = state.closedSinceStarted,
            loading = false,
            error = None
          )
      // Stays behind: frozen, with the error shown
      case Failed(_, error) =>
        state.copy(loading = false, error = Some(error))
end LiveModel
