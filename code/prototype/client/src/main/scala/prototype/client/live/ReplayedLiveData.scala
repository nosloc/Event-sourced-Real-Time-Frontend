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
  * those received during the load are not lost, and those already in the
  * snapshot are not applied twice (see `LiveModel.step`). Nothing runs until
  * one of its signals is read.
  *
  * K is the type of the key (for example a therapist), A the type of the value,
  * E the type of the events `decode` keeps.
  */
final class ReplayedLiveData[K, A, E](
    feed: EventBus[FeedMessage],
    key: Signal[K],
    load: K => IO[(A, Long)],
    decode: EventMessage => Option[E],
    evolve: (K, A, E) => A
) extends LiveData[A]:

  private val opened: Signal[Boolean] =
    feed.events
      .collect {
        case FeedMessage.Opened => true
        case FeedMessage.Closed => false
      }
      .startWith(false)
  // The key to load, each time a snapshot is needed: when the stream opens
  // (first time or reconnect), and when the key changes while it is open
  private val reloadKey: EventStream[K] =
    feed.events
      .collect { case FeedMessage.Opened => () }
      .sample(key)
      .mergeWith(
        key.changes.filterWith(opened)
      )

  private val receivedInput: EventStream[LiveModel.Input[K, A, E]] =
    feed.events
      .collect { case FeedMessage.Event(message) => message }
      .map(message =>
        decode(message).map(event => Input.Received(message.position, event))
      )
      .collectSome

  // The state of the live value: the loads (a status per reload, for the latest
  // key only) and the events received, folded by `LiveModel.step`
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
      .scanLeft(LiveModel.initial[K, A, E])(LiveModel.step(evolve))

  // Exposed state: read separately, so a failed reload can show its error
  // next to the value still on screen
  override val valueS: Signal[Option[A]] = stateS.map(_.value)
  override val loadingS: Signal[Boolean] = stateS.map(_.loading)
  override val errorS: Signal[Option[Throwable]] = stateS.map(_.error)

object ReplayedLiveData:

  /** A `ReplayedLiveData` for a value that does not depend on a key, such as a
    * list shown whole (the companies of the admin tab). It is a keyed one with
    * a constant key, so it loads, replays and reloads exactly the same way;
    * only the key is gone from `load` and `evolve`.
    */
  def keyless[A, E](
      feed: EventBus[FeedMessage],
      load: IO[(A, Long)],
      decode: EventMessage => Option[E],
      evolve: (A, E) => A
  ): ReplayedLiveData[Unit, A, E] =
    new ReplayedLiveData[Unit, A, E](
      feed = feed,
      key = Signal.fromValue(()),
      load = _ => load,
      decode = decode,
      evolve = (_, value, event) => evolve(value, event)
    )
