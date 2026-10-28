package prototype.client.live

import com.raquo.laminar.api.L.*
import cats.effect.IO
import prototype.client.Api
import prototype.api.EventView
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.live.LiveModel.Input

/** A value kept up to date from the server's events, for one key at a time.
  * Its snapshot is loaded once the stream has subscribed, and again after a
  * reconnect or a key change while the stream is open. The events received are
  * applied with `evolve`, in the order received: those received during the
  * load are not lost, and those already in the snapshot are not applied twice
  * (see `LiveModel.step`). Nothing runs until one of its signals is read.
  *
  * K is the type of the key (for example a therapist), A the type of the value,
  * E the type of the events `decode` keeps.
  */
final class LiveDataSource[K, A, E](
    feed: EventBus[FeedMessage],
    key: Signal[K],
    load: K => IO[(A, Long)],
    decode: EventView => Option[E],
    evolve: (K, A, E) => A
):

  private val opened: Signal[Boolean] =
    feed.events
      .collect {
        case FeedMessage.Opened => true
        case FeedMessage.Closed => false
      }
      .startWith(false)
  // Is reload needed
  private val reloadKey: EventStream[K] =
    feed.events
      .collect { case FeedMessage.Opened => () }
      .sample(key)
      .mergeWith(
        key.changes.filterWith(opened)
      )

  private def loadInput(k: K): EventStream[LiveModel.Input[K, A, E]] =
    EventStream
      .fromValue(Input.Started(k))
      .mergeWith(
        Api.stream(load(k)).map {
          case Right((value, position)) =>
            Input.Loaded(k, value, position)
          case Left(error) => Input.Failed(k, error)
        }
      )

  private val receivedInput: EventStream[LiveModel.Input[K, A, E]] =
    feed.events
      .collect { case FeedMessage.Event(view) => view }
      .map(view =>
        decode(view).map(event => Input.Received(view.position, event))
      )
      .collectSome

  // The state of the live value
  private val stateVar: Signal[LiveModel.State[K, A, E]] =
    reloadKey
      .flatMapSwitch(loadInput)
      .mergeWith(receivedInput)
      .scanLeft(LiveModel.initial[K, A, E])(LiveModel.step(evolve))

  // Exposed state: read separately, so a failed reload can show its error
  // next to the value still on screen
  val valuesS: Signal[Option[A]] = stateVar.signal.map(_.value)
  val loadingS: Signal[Boolean] = stateVar.signal.map(_.loading)
  val errorS: Signal[Option[Throwable]] = stateVar.signal.map(_.error)
