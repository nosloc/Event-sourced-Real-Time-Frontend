package prototype.client.live

import cats.effect.IO

import prototype.api.EventMessage
import prototype.client.Api
import prototype.client.live.LiveFeed.{FeedMessage, StreamStatus}
import prototype.client.live.LiveModel.Input
import prototype.client.live.ReplayedLiveData.{CheckInput, SyncStatus}

import com.raquo.laminar.api.L.*

/** A `LiveData` kept up to date by replaying the server's events in the
  * browser, for one key at a time. Its snapshot is loaded once the stream has
  * subscribed, and again after a reconnect or a key change while the stream is
  * open. The events received are applied with `evolve`, in the order received:
  * those received during the load are not lost, and those already in the
  * snapshot are not applied twice (see `LiveModel.step`). Nothing runs until
  * one of its signals is read.
  *
  * K is the type of the key (for example a therapist), A the type of the value,
  * E the type of the events `decode` keeps. `name` names it in the trace.
  */
final class ReplayedLiveData[K, A, E](
    feed: LiveFeed,
    name: String,
    key: Signal[K],
    load: K => IO[(A, Long)],
    decode: EventMessage => Option[E],
    evolve: (K, A, E) => A,
    same: (A, A) => Boolean = (a1: A, a2: A) => a1 == a2
) extends LiveData[A]:

  // The key to load, each time a snapshot is needed: when the stream opens
  // (first time or reconnect), and when the key changes while it is open
  private val reloadKey: EventStream[K] =
    feed.openings
      .sample(key)
      .mergeWith(
        key.changes.filterWith(feed.isOpenS)
      )

  private val receivedInput: EventStream[Input[K, A, E]] =
    feed.events
      .collect { case FeedMessage.Event(message) => message }
      .map(message =>
        decode(message).map(event =>
          feed.tracer.record(TraceStep.Kind.Decoded(name), message.position)
          Input.Received(message.position, event)
        )
      )
      .collectSome

  // The state of the live value: the loads (a status per reload, for the latest
  // key only) and the events received, folded by `LiveModel.step`. The trace
  // tells an event applied to the value (the position moved) from one only
  // kept aside: still loading, or already in the snapshot. It also records the
  // loads of the snapshot, with the position the snapshot includes
  private val stateS: Signal[LiveModel.State[K, A, E]] =
    reloadKey
      .flatMapWithStatus(k => Api.stream(load(k)))
      .map {
        case Pending(k) =>
          Input.Started(k)
        case Resolved(k, Right((value, position)), _) =>
          Input.Loaded(k, value, position)
        case Resolved(k, Left(error), _) =>
          Input.Failed(k, error)
      }
      .mergeWith(receivedInput)
      .scanLeft(LiveModel.initial[K, A, E])((state, input) =>
        val next = LiveModel.step(evolve)(state, input)
        input match
          case Input.Received(position, _) =>
            val applied = next.position > state.position
            feed.tracer.record(TraceStep.Kind.Folded(name, applied), position)
          case Input.Started(k) =>
            feed.tracer.record(
              TraceStep.Kind.SnapshotRequested(name, s"$name for $k")
            )
          case Input.Loaded(_, received, position) =>
            feed.tracer.record(
              TraceStep.Kind.SnapshotLoaded(name, position, sizeOf(received))
            )
          case Input.Failed(_, _) =>
            feed.tracer.record(TraceStep.Kind.SnapshotFailed(name))
        next
      )

  // The size of a snapshot, when it is a list. It walks the whole list, so it
  // is only called inside the trace's argument, built only while the
  // step-by-step mode is on
  private def sizeOf(value: A): Option[Int] =
    value match
      case list: Iterable[?] => Some(list.size)
      case _                 => None

  // Exposed state: read separately, so a failed reload can show its error
  // next to the value still on screen
  override val valueS: Signal[Option[A]] = stateS.map(_.value)
  override val loadingS: Signal[Boolean] = stateS.map(_.loading)
  override val errorS: Signal[Option[Throwable]] = stateS.map(_.error)

  // Re-runs on every check of the stream (every 2 s): the stream's statuses
  // are not deduplicated, so an `UpToDate` at the same head checks again
  private val checkInputs: EventStream[CheckInput[A]] =
    feed.streamStatusS.changes
      .withCurrentValueOf(key)
      .flatMapSwitch {
        case (StreamStatus.UpToDate(_), k) =>
          Api
            .stream(load(k).map((value, _) => value))
            .withCurrentValueOf(stateS)
            .map((result, state) => CheckInput.Loaded(result, state.value))
        case (stream, _) =>
          EventStream.fromValue(CheckInput.Waiting(stream))
      }

  /** Whether the source matches the server's, once the stream is up to date. An
    * `Inconsistent` seen once may be a read model still catching up; one that
    * lasts several checks (`checks`) is a real difference.
    */
  val statusS: Signal[SyncStatus[A]] =
    checkInputs.scanLeft(SyncStatus.NotChecked: SyncStatus[A]) {
      case (_, CheckInput.Waiting(stream)) =>
        SyncStatus.WaitingForStream(stream)
      case (_, CheckInput.Loaded(Left(error), _)) =>
        SyncStatus.CheckFailed(error)
      case (previous, CheckInput.Loaded(Right(server), local)) =>
        if local.exists(same(_, server)) then SyncStatus.Synced
        else
          val checks = previous match
            case SyncStatus.Inconsistent(_, _, checks) => checks + 1
            case _                                     => 1
          SyncStatus.Inconsistent(local, server, checks)
    }

object ReplayedLiveData:

  /** A `ReplayedLiveData` for a value that does not depend on a key, such as a
    * list shown whole (the companies of the admin tab). It is a keyed one with
    * a constant key, so it loads, replays and reloads exactly the same way;
    * only the key is gone from `load` and `evolve`.
    */
  def keyless[A, E](
      feed: LiveFeed,
      name: String,
      load: IO[(A, Long)],
      decode: EventMessage => Option[E],
      evolve: (A, E) => A,
      same: (A, A) => Boolean = (a1: A, a2: A) => a1 == a2
  ): ReplayedLiveData[Unit, A, E] =
    new ReplayedLiveData[Unit, A, E](
      feed = feed,
      name = name,
      key = Signal.fromValue(()),
      load = _ => load,
      decode = decode,
      evolve = (_, value, event) => evolve(value, event),
      same = same
    )

  enum SyncStatus[+A]:
    case NotChecked
    case WaitingForStream(stream: StreamStatus)
    case Synced
    case CheckFailed(error: Throwable)
    case Inconsistent(local: Option[A], server: A, checks: Int)

  // What a check of the source reacts to: the stream not up to date (nothing
  // can be concluded), or the server's value read while it is
  private enum CheckInput[+A]:
    case Waiting(stream: StreamStatus)
    case Loaded(result: Either[Throwable, A], local: Option[A])
