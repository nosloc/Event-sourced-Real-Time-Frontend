package prototype.client.live

import cats.effect.IO

import prototype.api.EventMessage
import prototype.client.Api
import prototype.client.live.LiveFeed.FeedMessage

import com.raquo.laminar.api.L.*

/** A `LiveData` kept up to date by reloading it from the server, for one key
  * at a time: once the stream has subscribed, again after a reconnect or a key
  * change while the stream is open, and on every event `decode` keeps. The
  * events themselves are not applied: they only say that the value is out of
  * date. While a reload runs, and after it fails, the previous value stays; a
  * new key drops it. Nothing runs until one of its signals is read.
  *
  * For now a new reload cancels the one in flight, and the position returned
  * by `load` is not used, so a reload may not include the event that caused
  * it.
  *
  * K is the type of the key (for example a company), A the type of the value,
  * E the type of the events `decode` keeps.
  */
final class RefreshedLiveData[K, A, E](
    feed: EventBus[FeedMessage],
    key: Signal[K],
    load: K => IO[(A, Long)],
    decode: EventMessage => Option[E]
) extends LiveData[A]:

  private case class State(
      key: Option[K],
      value: Option[A],
      loading: Boolean,
      error: Option[Throwable]
  )
  private val initialState: State = State(None, None, false, None)

  private val opened: Signal[Boolean] =
    feed.events
      .collect {
        case FeedMessage.Opened => true
        case FeedMessage.Closed => false
      }
      .startWith(false)

  // The key to load, each time a refresh is needed: when the stream opens
  // (first time or reconnect), when an event `decode` keeps arrives, and when
  // the key changes while the stream is open
  private val refreshKey: EventStream[K] =
    feed.events
      .withCurrentValueOf(key)
      .collect {
        case (FeedMessage.Opened, k)                                      => k
        case (FeedMessage.Event(message), k) if decode(message).isDefined => k
      }
      .mergeWith(key.changes.filterWith(opened))

  private val stateS: Signal[State] =
    refreshKey
      .flatMapWithStatus(k => Api.stream(load(k)))
      .scanLeft(initialState) { (state, status) =>
        status match
          case Pending(k) if state.key.contains(k) =>
            state.copy(loading = true, error = None)
          case Pending(k) =>
            state.copy(
              key = Some(k),
              value = None,
              loading = true,
              error = None
            )
          case Resolved(_, Right((value, _)), _) =>
            state.copy(value = Some(value), loading = false, error = None)
          case Resolved(_, Left(error), _) =>
            state.copy(loading = false, error = Some(error))
      }

  override val valueS: Signal[Option[A]] =
    stateS.map(_.value)
  override val loadingS: Signal[Boolean] =
    stateS.map(_.loading)
  override val errorS: Signal[Option[Throwable]] =
    stateS.map(_.error)
