package prototype.client.live

import prototype.client.live.LiveFeed.StreamStatus
import prototype.client.live.SyncStatus.Check

/** The rules of the check of one source against the server, pure (no timing):
  * `SyncCheck` feeds it and runs the reads it asks for.
  *
  * A comparison means something only when the stream is up to date, the source
  * is not loading, and the source did not change between the read and its
  * answer (so the server's value is compared with the browser's value of the
  * same moment). One read at a time; a read is asked again only when something
  * moved since the last match (the store's head, the key or the source's
  * version), so a quiet page sends no request.
  *
  * K is the type of the key, A the type of the value.
  */
object SyncCheckModel:

  /** The source as the check sees it. `version` changes whenever the value may
    * have changed (the position of the last event applied, or a count of the
    * changes of a derived value).
    */
  final case class Local[K, A](
      key: Option[K],
      loading: Boolean,
      version: Long,
      value: Option[A]
  )

  enum Input[K, A]:
    // A check of the stream, with the source as it is now
    case Tick(stream: StreamStatus, local: Local[K, A])
    // The answer of a read of the server's value, with the source as it is now
    case Answered(key: K, result: Either[Throwable, A], local: Local[K, A])

  enum Phase[+K]:
    case Idle
    // A read for `key` is running, asked at `version`; `ticks` counts the
    // stream's checks since, to give up on a read whose answer was lost
    case InFlight(key: K, version: Long, ticks: Int)

  /** @param check
    *   where the check stands, shown by the badge
    * @param phase
    *   whether a read is running
    * @param head
    *   the store's head of the last tick, while the stream is up to date
    * @param syncedAt
    *   the head, key and version of the last match: nothing to read again while
    *   none of them moves
    */
  final case class State[K](
      check: Check,
      phase: Phase[K],
      head: Option[Long],
      syncedAt: Option[(Long, K, Long)]
  )

  def initial[K]: State[K] =
    State(Check.NotChecked, Phase.Idle, None, None)

  // A read with no answer after this many ticks is given up (30 s at 2 s)
  val MaxTicksInFlight = 15

  // Differences in a row before a difference counts as real
  val ConfirmAfter = 2

  /** The next state, and the key to read the server's value for, if any. */
  def step[K, A](same: Same[A])(
      state: State[K],
      input: Input[K, A]
  ): (State[K], Option[K]) =
    input match
      case Input.Tick(StreamStatus.UpToDate(head), local) =>
        (local.key, local.value) match
          case (Some(key), Some(_)) if !local.loading =>
            state.phase match
              case Phase.InFlight(k, version, ticks)
                  if ticks < MaxTicksInFlight =>
                (
                  state.copy(
                    phase = Phase.InFlight(k, version, ticks + 1),
                    head = Some(head)
                  ),
                  None
                )
              case _ =>
                if state.syncedAt.contains((head, key, local.version)) then
                  (state.copy(head = Some(head)), None)
                else
                  (
                    state.copy(
                      phase = Phase.InFlight(key, local.version, 0),
                      head = Some(head)
                    ),
                    Some(key)
                  )
          case _ =>
            (state.copy(check = Check.Loading, head = Some(head)), None)

      // Not up to date: nothing can be concluded, now or from a read running
      case Input.Tick(stream, _) =>
        (state.copy(check = Check.WaitingForStream(stream), head = None), None)

      case Input.Answered(key, result, local) =>
        state.phase match
          case Phase.InFlight(asked, version, _) if asked == key =>
            val idle = state.copy(phase = Phase.Idle)
            (state.head, result) match
              // The stream left `UpToDate` meanwhile
              case (None, _) => (idle, None)

              case (Some(_), Left(error)) =>
                val failures = state.check match
                  case Check.Failed(_, failures) => failures + 1
                  case _                         => 1
                (
                  idle.copy(
                    check = Check.Failed(error, failures),
                    syncedAt = None
                  ),
                  None
                )

              // The source changed during the read: compared with a value of
              // another moment, it would mean nothing. The next tick reads again
              case (Some(_), Right(_))
                  if !local.key.contains(key) || local.loading ||
                    local.version != version =>
                (idle.copy(syncedAt = None), None)

              case (Some(head), Right(server)) =>
                local.value match
                  case None        => (idle, None)
                  case Some(value) =>
                    same.diff(value, server) match
                      case None =>
                        (
                          idle.copy(
                            check = Check.Synced,
                            syncedAt = Some((head, key, version))
                          ),
                          None
                        )
                      case Some(diff) =>
                        val checks = state.check match
                          case Check.Suspect(_)        => 2
                          case Check.Diverged(_, done) => done + 1
                          case _                       => 1
                        val check =
                          if checks >= ConfirmAfter then
                            Check.Diverged(diff, checks)
                          else Check.Suspect(diff)
                        (idle.copy(check = check, syncedAt = None), None)

          // The answer of a read given up, or for a key no longer asked
          case _ => (state, None)
end SyncCheckModel
