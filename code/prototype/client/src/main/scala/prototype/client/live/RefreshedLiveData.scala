package prototype.client.live

import cats.effect.IO

import prototype.api.EventMessage
import prototype.client.Api
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.live.RefreshModel.Input

import com.raquo.laminar.api.L.*

/** A `LiveData` kept up to date by reloading it from the server, for one key at
  * a time: once the stream has subscribed (the first load waits for it, and
  * again after a reconnect), when the key changes, and on every event
  * `triggerRefresh` accepts. The events themselves are not applied: they only
  * say that the value is out of date. The rules are `RefreshModel.step`: one
  * load in flight plus one pending (a burst of events gives at most two loads), an answer for
  * another key is ignored, and an answer older than the value on screen does
  * not replace it. While a reload runs, and after it fails, the previous value
  * stays; a new key drops it, unless `invalidatePrevious(old, new)` is false:
  * the old value still stands for the new key while it loads (for example the
  * same company over another period).
  *
  * Nothing runs until one of its signals is read, and `keepLoaded` is optional:
  * the loads run inside the chain that the signals read. If nothing reads it
  * while a load is in flight, that load's answer is lost and the model stays
  * waiting for it, so build a new instance rather than reading a stopped one
  * again (the dashboards build one per company).
  *
  * K is the type of the key (for example a company), A the type of the value, E
  * the type of the events (unused for now).
  */
final class RefreshedLiveData[K, A, E](
    feed: EventBus[FeedMessage],
    key: Signal[K],
    load: K => IO[(A, Long)],
    triggerRefresh: EventMessage => Boolean,
    invalidatePrevious: (K, K) => Boolean = (_: K, _: K) => true
) extends LiveData[A]:

  // The stream opened, with the key at that moment: the first load waits for
  // it, so that no event falls between the read and the subscription
  private val openedInput: EventStream[Input[K, A]] =
    feed.events.withCurrentValueOf(key).collect {
      case (FeedMessage.Opened, k) => Input.Opened(k)
    }

  // Each new key (`changes` skips the first one, which comes with `Opened`)
  private val keyChangedInput: EventStream[Input[K, A]] =
    key.distinct.changes.map(Input.KeyChanged(_))

  private val eventArrivedInput: EventStream[Input[K, A]] =
    feed.events.collect {
      case FeedMessage.Event(message) if triggerRefresh(message) =>
        Input.EventArrived(message.position)
    }

  // The answers of the loads, sent back into the model by `runLoads`
  private val loadResults: EventBus[Input[K, A]] = new EventBus

  private val stepResult: Signal[(RefreshModel.State[K, A], Option[K])] =
    EventStream
      .merge(
        openedInput,
        keyChangedInput,
        eventArrivedInput,
        loadResults.events
      )
      .scanLeft[(RefreshModel.State[K, A], Option[K])](
        (RefreshModel.initial[K, A], None)
      )((acc, input) => RefreshModel.step(invalidatePrevious)(acc._1, input))

  // Runs each load the model asks for and sends its answer back into the model
  // (a new transaction). It never emits: it is only there so that the loads
  // run while the state is read, without `keepLoaded`
  private val runLoads: Signal[Unit] =
    stepResult.changes
      .collect { case (_, Some(k)) => k }
      .flatMapSwitch(k => Api.stream(load(k)).map(k -> _))
      .tapEach {
        case (k, Right((value, position))) =>
          loadResults.emit(Input.Loaded(k, value, position))
        case (k, Left(error)) =>
          loadResults.emit(Input.Failed(k, error))
      }
      .filter(_ => false)
      .mapToUnit
      .startWith(())

  private val stateS: Signal[RefreshModel.State[K, A]] =
    stepResult.combineWithFn(runLoads)((result, _) => result._1)

  override val valueS: Signal[Option[A]] = stateS.map(_.value)
  override val loadingS: Signal[Boolean] = stateS.map(_.loading)
  override val errorS: Signal[Option[Throwable]] = stateS.map(_.error)
