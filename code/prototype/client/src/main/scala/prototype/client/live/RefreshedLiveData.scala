package prototype.client.live

import cats.effect.IO

import prototype.api.{Cursor, EventMessage}
import prototype.client.Api
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.live.RefreshModel.{Input, Outcome}

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
  * @param name
  *   which live data this is in the feed's trace: the loads are numbered per
  *   instance, so a number alone does not say whose load it is
  * @param key
  *   the signal of the key
  * @param load
  *   how to get the refreshed value, and the corresponding global position
  * @param triggerRefresh
  *   tell if an event message trigger a refresh or not
  * @param invalidatePrevious
  *   tell if a new key invalidate the previous shown value (delete it from the
  *   screen), or keep it until the `load` completes
  * @param same
  *   compares it with a new read, for `syncS`: a difference while no reload is
  *   running is an event `triggerRefresh` missed
  */
final class RefreshedLiveData[K, A](
    feed: LiveFeed,
    name: String,
    key: Signal[K],
    load: K => IO[(A, Cursor)],
    triggerRefresh: EventMessage => Boolean,
    invalidatePrevious: (K, K) => Boolean = (_: K, _: K) => true,
    same: Same[A] = Same.equal[A]
) extends LiveData[A]:

  // When the stream opened, input it with the current value of the key
  private val openedInput: EventStream[Input[K, A]] =
    feed.openings.withCurrentValueOf(key).map(Input.Opened(_))

  // Each new key input (`changes` skips the first one, which comes with `Opened`)
  private val keyChangedInput: EventStream[Input[K, A]] =
    key.distinct.changes.map(Input.KeyChanged(_))

  // Keep only the events that triggers a refresh
  private val eventArrivedInput: EventStream[Input[K, A]] =
    feed.events.collect { case FeedMessage.Event(message) =>
      if triggerRefresh(message) then
        feed.tracer.record(
          TraceStep.Kind.Considered(name),
          message.cursor.position
        )
        Some(Input.EventArrived(message.cursor))
      else
        feed.tracer.record(
          TraceStep.Kind.NotConsidered(name),
          message.cursor.position
        )
        None
    }.collectSome

  // The answers of the loads, sent back into the model by `runLoads`
  private val loadResults: EventBus[Input[K, A]] = new EventBus

  // Return a signal of:
  // _1. the state of the refresh model
  // _2. What the step decided (a load to launch, an answer, ...), or None if nothing
  private val stepResult
      : Signal[(RefreshModel.State[K, A], Option[Outcome[K, A]])] =
    // All the inputs merged in a single EventStream
    EventStream
      .merge(
        openedInput,
        keyChangedInput,
        eventArrivedInput,
        loadResults.events
      )
      .scanLeft[(RefreshModel.State[K, A], Option[Outcome[K, A]])](
        (RefreshModel.initial[K, A], None)
      )((acc, input) =>
        val result = RefreshModel.step(invalidatePrevious)(acc._1, input)
        // Recorded here, once per step: the signal itself can be read again
        result._2.foreach(outcome =>
          feed.tracer.record(
            TraceStep.Kind.Reload(name, outcome),
            eventBehind(outcome)
          )
        )
        result
      )

  // Runs each load the model asks for and sends its answer back.
  // It never emits: it is only there so that the loads
  // run while the state is read, without `keepLoaded`
  private val runLoads: Signal[Unit] =
    stepResult.changes
      .collect {
        case (_, Some(Outcome.InitialLoad(k, _)))               => k
        case (_, Some(Outcome.Load(k, _, _)))                   => k
        case (_, Some(Outcome.AnswerAndLaunchPending(_, load))) => load.key
        case (_, Some(Outcome.ErrorAndLaunchPending(_, load)))  => load.key
      }
      .flatMapSwitch(k => Api.stream(load(k)).map(k -> _))
      .tapEach {
        case (k, Right((value, cursor))) =>
          loadResults.emit(Input.Loaded(k, value, cursor))
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

  /** Compared with a new read once no reload runs: see `SyncCheck`. The
    * source's version is the position of the value shown (compared for
    * equality only, and positions are unique).
    */
  override val syncS: Signal[SyncStatus] =
    SyncCheck(
      feed,
      name,
      stateS.map(state =>
        SyncCheckModel.Local(
          state.key,
          state.loading || state.phase != RefreshModel.Phase.Idle,
          state.cursor.position,
          state.value
        )
      ),
      k => load(k).map((value, _) => value),
      same
    )

  // The event an outcome belongs to in the trace (by its position): the one
  // that launched the load or was batched, the highest one batched for a
  // pending load; None for an answer alone and for a load with no event
  // behind it
  private def eventBehind(outcome: Outcome[K, A]): Option[Long] =
    outcome match
      case Outcome.Load(_, cursor, _)                 => Some(cursor.position)
      case Outcome.Batched(cursor, _)                 => Some(cursor.position)
      case Outcome.AnswerAndLaunchPending(_, load)    => Some(load.cursor.position)
      case Outcome.ErrorAndLaunchPending(_, load)     => Some(load.cursor.position)
      case Outcome.InitialLoad(_, _)                  => None
      case Outcome.Answer(_, _, _) | Outcome.Error(_) => None
