package prototype.client.live

import cats.effect.IO

import prototype.api.EventMessage
import prototype.client.Api
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.live.RefreshModel.Input

import com.raquo.laminar.api.L.*

/** A `LiveData` kept up to date by reloading it from the server, for one key at
  * a time.
  *
  * The data is reloaded once the stream has subscribed, when the key changes,
  * and on every event `triggerRefresh` accepts. The events themselves are not
  * applied: they only say that the value is out of date. The rules are
  * `RefreshModel.step`.
  *
  * Nothing runs until one of its signals is read, and `keepLoaded` is optional
  * and can be used to keep the value up to date even if the signal isn't read.
  * If nothing reads it while a load is in flight, that load's answer is lost
  * and the model stays waiting for it, so build a new instance rather than
  * reading a stopped one again (the dashboards build one per company).
  *
  * K is the type of the key (for example a company), A the type of the value.
  *
  * @param feed
  *   the real time feed of events
  * @param key
  *   the signal of the key
  * @param load
  *   how to get the refreshed value, and the corresponding global position
  * @param triggerRefresh
  *   tell if an event message trigger a refresh or not
  * @param invalidatePrevious
  *   tell if a new key invalidate the previous shown value (delete it from the
  *   screen), or keep it until the `load` completes
  */
final class RefreshedLiveData[K, A](
    feed: EventBus[FeedMessage],
    key: Signal[K],
    load: K => IO[(A, Long)],
    triggerRefresh: EventMessage => Boolean,
    invalidatePrevious: (K, K) => Boolean = (_: K, _: K) => true
) extends LiveData[A]:

  // When the stream opened, input it with the current value of the key
  private val openedInput: EventStream[Input[K, A]] =
    feed.events.withCurrentValueOf(key).collect {
      case (FeedMessage.Opened, k) => Input.Opened(k)
    }

  // Each new key input (`changes` skips the first one, which comes with `Opened`)
  private val keyChangedInput: EventStream[Input[K, A]] =
    key.distinct.changes.map(Input.KeyChanged(_))

  // Keep only the events that triggers a refresh
  private val eventArrivedInput: EventStream[Input[K, A]] =
    feed.events.collect {
      case FeedMessage.Event(message) if triggerRefresh(message) =>
        Input.EventArrived(message.position)
    }

  // The answers of the loads, sent back into the model by `runLoads`
  private val loadResults: EventBus[Input[K, A]] = new EventBus

  // Return a signal of:
  // _1. the state of the refresh model
  // _2. An option saying if a reload is need (Some(key)), or nothing is needed (None)
  private val stepResult: Signal[(RefreshModel.State[K, A], Option[K])] =
    // All the inputs merged in a single EventStream
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

  // Runs each load the model asks for and sends its answer back.
  // It never emits: it is only there so that the loads
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

  // Runs the loads whenever the state is read
  private val stateS: Signal[RefreshModel.State[K, A]] =
    stepResult.combineWithFn(runLoads)((result, _) => result._1)

  override val valueS: Signal[Option[A]] = stateS.map(_.value)
  override val loadingS: Signal[Boolean] = stateS.map(_.loading)
  override val errorS: Signal[Option[Throwable]] = stateS.map(_.error)
