package prototype.client.live

import cats.effect.IO

import prototype.api.EventMessage
import prototype.client.Api
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.live.LiveModel.Input

import com.raquo.laminar.api.L.*

/** A `LiveData` kept up to date by replaying the server's events in the
  * browser, for one key at a time. Its snapshot is loaded once the stream has
  * subscribed, and again after a reconnect or a key change while the stream is
  * open. The events received are applied with `evolve`, in the order received:
  * those received during the load are kept aside (the last 256), those already
  * in the snapshot are not applied twice, and a snapshot whose later events
  * were dropped is loaded again until one is complete (see `LiveModel.step`).
  * Nothing runs until one of its signals is read.
  *
  * K is the type of the key (for example a therapist), A the type of the value,
  * E the type of the events `decode` keeps. `name` names it in the trace and in
  * its `syncS`; `same` compares it with the server's.
  */
final class ReplayedLiveData[K, A, E](
    feed: LiveFeed,
    name: String,
    key: Signal[K],
    load: K => IO[(A, Long)],
    decode: EventMessage => Option[E],
    evolve: (K, A, E) => A,
    same: Same[A] = Same.equal[A]
) extends LiveData[A]:

  // A snapshot rejected by `LiveModel.step` asks for a new load here. The bus breaks the loop
  private val refetchBus = new EventBus[K]

  private val reloadKey: EventStream[K] =
    feed.openings
      .sample(key)
      .mergeWith(
        key.changes.filterWith(feed.isOpenS),
        refetchBus.events.filterWith(feed.isOpenS)
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
          case Input.Loaded(k, received, position) =>
            feed.tracer.record(
              TraceStep.Kind.SnapshotLoaded(name, position, sizeOf(received))
            )
            // Rejected and retried: the counter goes up only then. The write
            // runs in a new transaction, after this one
            if next.refetches > state.refetches then refetchBus.writer.onNext(k)
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

  /** Compared with a new read of the snapshot, by `same`: see `SyncCheck`. The
    * source's version is the position of the last event applied.
    */
  override val syncS: Signal[SyncStatus] =
    SyncCheck(
      feed,
      name,
      stateS.map(state =>
        SyncCheckModel.Local(
          state.key,
          state.loading,
          state.position,
          state.value
        )
      ),
      k => load(k).map((value, _) => value),
      same
    )

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
      same: Same[A] = Same.equal[A]
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
