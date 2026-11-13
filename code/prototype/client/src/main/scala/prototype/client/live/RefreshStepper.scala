package prototype.client.live

import prototype.client.live.RefreshModel.Outcome
import prototype.client.live.TraceStep.Kind

// `source` is the name of the live data this stepper follows, the same as the
// `name` of its `RefreshedLiveData`: its loads are numbered per live data.
// `ignoredCount` is the number of `Ignored` entries in `history`, so that
// what is left to show is known without reading the history
final case class RefreshStepper[A](
    source: String,
    currentStep: RefreshStepper.Shown,
    pipeline: RefreshStepper.Pipeline[A],
    history: Vector[RefreshStepper.Entry],
    ignoredCount: Int,
    displayed: Option[A]
):
  import RefreshStepper.*

  // A Next does something while a trace step is left to show, a load waits
  // to be displayed, or a step left something behind
  def hasNextStep(skipIgnored: Boolean): Boolean =
    stepsLeft(skipIgnored) > 0 || pipeline.hasAnswered ||
      pipeline.hasAutomaticStepPending

  // The trace steps left to show: an ignored event is two steps when shown,
  // none when skipped
  def stepsLeft(skipIgnored: Boolean): Int =
    if skipIgnored then history.size - ignoredCount
    else history.size + ignoredCount

  def step(skipIgnored: Boolean): RefreshStepper[A] =
    if !hasNextStep(skipIgnored) then this
    // A load answered is displayed on a step of its own, before the next
    // trace step: it is the one that changes the screen. No trace step is
    // behind it. Answers come one at a time, so there is one load to name,
    // and its value is the one now on screen
    else if pipeline.hasAnswered then
      val (number, value) = pipeline.loads
        .collect { case (number, Load(LoadPlace.Answered(_, value), _)) =>
          (number, value)
        }
        .maxBy(_._1)
      copy(
        currentStep = Shown.Display(number),
        pipeline = pipeline.automaticStep().displayAnswered(),
        displayed = Some(value)
      )
    else
      // When skipped, the ignored events at the head leave the history for
      // good: each one is dropped once, however long the burst
      val remaining =
        if skipIgnored then
          history.dropWhile {
            case Entry.Ignored(_, _) => true
            case Entry.Step(_)       => false
          }
        else history
      val count = ignoredCount - (history.size - remaining.size)
      remaining.headOption match
        // No trace step left: the Next only removes what the last step left
        // behind, so that the last load displayed does not stay forever
        case None =>
          copy(
            currentStep = Shown.Idle,
            pipeline = pipeline.automaticStep(),
            history = remaining,
            ignoredCount = count
          )
        case Some(Entry.Step(traceStep)) =>
          showing(traceStep, remaining.tail, count)
        // An ignored event shown takes two Nexts: its arrival now, its "not
        // considered" next, as an ordinary step
        case Some(Entry.Ignored(arrival, notConsidered)) =>
          showing(
            arrival,
            Entry.Step(notConsidered) +: remaining.tail,
            count - 1
          )

  // Only the steps of this stepper are kept: the ones of other live data are
  // never shown here
  def addStep(step: TraceStep): RefreshStepper[A] =
    if !concerns(step) then this
    else
      (step.kind, history.lastOption) match
        // Nothing of this source came between the event's arrival and its
        // end: the event never reached its live data, it is not shown at all
        case (Kind.Ended, Some(Entry.Step(arrival)))
            if isArrivalOf(arrival, step) =>
          copy(history = history.init)
        case (Kind.Ended, _) => this
        // The event that just arrived is one this dashboard ignores: its
        // arrival and its "not considered" become one entry, skipped as a
        // whole. Both are recorded in the same message, so they are next to
        // each other
        case (Kind.NotConsidered(_), Some(Entry.Step(arrival)))
            if isArrivalOf(arrival, step) =>
          copy(
            history = history.init :+ Entry.Ignored(arrival, step),
            ignoredCount = ignoredCount + 1
          )
        case _ => copy(history = history :+ Entry.Step(step))

  private def isArrivalOf(arrival: TraceStep, step: TraceStep): Boolean =
    arrival.kind == Kind.Received && arrival.position == step.position

  private def showing(
      traceStep: TraceStep,
      rest: Vector[Entry],
      count: Int
  ): RefreshStepper[A] =
    RefreshStepper(
      source,
      Shown.Trace(traceStep),
      updatedPipeline(pipeline, traceStep),
      rest,
      count,
      displayed
    )

  // Whether a step is one of this stepper's: the arrival and the end of
  // every event, and the steps of its own source. The end is never shown: it
  // only tells `addStep` that the event's steps are all recorded
  private def concerns(traceStep: TraceStep): Boolean =
    traceStep.kind match
      case Kind.Received                  => true
      case Kind.Ended                     => true
      case Kind.Considered(stepSource)    => stepSource == source
      case Kind.NotConsidered(stepSource) => stepSource == source
      case Kind.Reload(stepSource, _)     => stepSource == source
      case Kind.Decoded | Kind.Folded(_)  => false
      case Kind.FiguresComputed(_)        => false

  private def updatedPipeline(
      pipeline: Pipeline[A],
      consideredStep: TraceStep
  ): Pipeline[A] =
    // What the previous step left behind is removed first, so that it stays
    // on screen for one step
    val cleaned = pipeline.automaticStep()
    val position = consideredStep.position
    consideredStep.kind match
      case Kind.Received      => cleaned.moved(position, EventPlace.Received)
      case Kind.Considered(_) => cleaned.moved(position, EventPlace.Considered)
      case Kind.NotConsidered(_) =>
        cleaned.moved(position, EventPlace.NotConsidered)
      case Kind.Reload(_, outcome) =>
        outcome match
          case Outcome.InitialLoad(_, loadNumber) =>
            cleaned.handleInitialLoad(loadNumber)
          case Outcome.Load(_, pos, loadNumber) =>
            cleaned.handleLoad(pos, loadNumber)
          case Outcome.Batched(pos, servedBy) =>
            cleaned.handleBatched(pos, servedBy)
          // The one cast of the stepper: the trace is untyped, but `concerns`
          // only lets in the reloads of this stepper's source, a single
          // `RefreshedLiveData` whose values are `A`
          case Outcome.Answer(pos, loadNumber, value) =>
            cleaned.handleAnswer(loadNumber, pos, value.asInstanceOf[A])
          case Outcome.AnswerAndLaunchPending(answer, load) =>
            cleaned
              .handleAnswer(
                answer.loadNumber,
                answer.position,
                answer.value.asInstanceOf[A]
              )
              .handleLoad(load.position, load.loadNumber)
          case Outcome.Error(loadNumber) =>
            cleaned.handleError(loadNumber)
          case Outcome.ErrorAndLaunchPending(error, load) =>
            cleaned
              .handleError(error.loadNumber)
              .handleLoad(load.position, load.loadNumber)
      case _ => cleaned

object RefreshStepper:

  // Where an event is before it is attached to a load
  enum EventPlace:
    case Received
    case Considered
    case NotConsidered

  // Where a load is: owed (`Pending`, its events are the ones batched), running,
  // or done
  enum LoadPlace[+A]:
    case Pending
    case InFlight
    // `position` is how far the server data goes, `value` what it loaded
    case Answered(position: Long, value: A)
    // The answer is on screen: a step of its own after `Answered`
    case Displayed
    case Failed
    case Cancelled

  // A load and the events it serves
  final case class Load[+A](place: LoadPlace[A], events: Set[Long])

  /** Where the events of one live data are: in `events` until a load takes
    * them, then in the load that serves them. An event is in one place only.
    */
  final case class Pipeline[A](
      events: Map[Long, EventPlace],
      loads: Map[Int, Load[A]]
  ):

    // Puts the event at `position` in `place`, nothing for a step with no event
    def moved(position: Option[Long], place: EventPlace): Pipeline[A] =
      position.fold(this)(pos => copy(events = events.updated(pos, place)))

    // Load `loadNumber` is launched: it takes the event at `position`, and
    // keeps the events batched for it
    def handleLoad(position: Long, loadNumber: Int): Pipeline[A] =
      val load = loadOrPending(loadNumber)
      Pipeline(
        events - position,
        loads.updated(
          loadNumber,
          Load(LoadPlace.InFlight, load.events + position)
        )
      )

    // The event at `position` waits for load `servedBy`, not launched yet
    def handleBatched(position: Long, servedBy: Int): Pipeline[A] =
      val load = loadOrPending(servedBy)
      Pipeline(
        events - position,
        loads.updated(servedBy, load.copy(events = load.events + position))
      )

    def handleAnswer(loadNumber: Int, position: Long, value: A): Pipeline[A] =
      withLoadPlace(loadNumber, LoadPlace.Answered(position, value))

    def handleError(loadNumber: Int): Pipeline[A] =
      withLoadPlace(loadNumber, LoadPlace.Failed)

    // A value loaded from scratch (first open, key change, reconnect): the loads
    // in flight are cancelled, the events not yet in a load are dropped, and
    // load `loadNumber` keeps the events batched for it
    def handleInitialLoad(loadNumber: Int): Pipeline[A] =
      val cancelled = loads.map((number, load) =>
        if load.place == LoadPlace.InFlight then
          (number, load.copy(place = LoadPlace.Cancelled))
        else (number, load)
      )
      val batched = loadOrPending(loadNumber).events
      Pipeline(
        Map.empty,
        cancelled.updated(loadNumber, Load(LoadPlace.InFlight, batched))
      )

    // Whether a load is answered and waits to be displayed
    def hasAnswered: Boolean =
      loads.values.exists(
        _.place match
          case LoadPlace.Answered(_, _) => true
          case _                        => false
      )

    // Whether a step left something behind for `automaticStep` to remove
    def hasAutomaticStepPending: Boolean =
      loads.values.exists(
        _.place match
          case LoadPlace.Pending | LoadPlace.InFlight |
              LoadPlace.Answered(_, _) =>
            false
          case LoadPlace.Displayed | LoadPlace.Failed | LoadPlace.Cancelled =>
            true
      ) || events.exists((_, place) => place == EventPlace.NotConsidered)

    // The loads answered are put on screen
    def displayAnswered(): Pipeline[A] =
      copy(loads =
        loads.map((number, load) =>
          load.place match
            case LoadPlace.Answered(_, _) =>
              (number, load.copy(place = LoadPlace.Displayed))
            case _ => (number, load)
        )
      )

    // What a step left behind is removed on the next one: an event not
    // considered, and a load done, with its events. A load answered stays until
    // it is displayed
    def automaticStep(): Pipeline[A] =
      Pipeline(
        events.filter((_, place) => place != EventPlace.NotConsidered),
        loads.filter((_, load) =>
          load.place match
            case LoadPlace.Pending | LoadPlace.InFlight |
                LoadPlace.Answered(_, _) =>
              true
            case LoadPlace.Displayed | LoadPlace.Failed | LoadPlace.Cancelled =>
              false
        )
      )

    // A load not seen yet (batched for, or started before the stepper) is
    // pending with no event
    private def loadOrPending(loadNumber: Int): Load[A] =
      loads.getOrElse(loadNumber, Load(LoadPlace.Pending, Set.empty))

    private def withLoadPlace(
        loadNumber: Int,
        place: LoadPlace[A]
    ): Pipeline[A] =
      copy(loads =
        loads.updated(loadNumber, loadOrPending(loadNumber).copy(place = place))
      )

  object Pipeline:
    def empty[A]: Pipeline[A] = Pipeline(Map.empty, Map.empty)

  // What the last Next showed: nothing yet (or only a clean-up), a trace step,
  // or the figures of a load put on screen, a step with no trace step behind
  enum Shown:
    case Idle
    case Trace(step: TraceStep)
    case Display(loadNumber: Int)

  // One entry of the history: a trace step, or an event this dashboard
  // ignores (its arrival and its "not considered", merged when the second is
  // recorded)
  enum Entry:
    case Step(step: TraceStep)
    case Ignored(arrival: TraceStep, notConsidered: TraceStep)

  def empty[A](source: String): RefreshStepper[A] =
    RefreshStepper[A](
      source,
      Shown.Idle,
      Pipeline.empty,
      Vector.empty,
      0,
      None
    )
