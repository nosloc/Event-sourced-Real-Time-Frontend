package prototype.client.live

import prototype.client.live.TraceStep.Kind
import prototype.client.live.RefreshModel.Outcome

// Where an event is before it is attached to a load
enum EventPlace:
  case Received
  case Considered
  case NotConsidered

// Where a load is: owed (`Pending`, its events are the ones batched), running,
// or done
enum LoadPlace:
  case Pending
  case InFlight
  // `position` is how far the server data goes
  case Answered(position: Long)
  // The answer is on screen: a step of its own after `Answered`
  case Displayed
  case Failed
  case Cancelled

// A load and the events it serves
case class Load(place: LoadPlace, events: Set[Long])

/** Where the events of one live data are: in `events` until a load takes them,
  * then in the load that serves them. An event is in one place only.
  */
case class Pipeline(events: Map[Long, EventPlace], loads: Map[Int, Load]):

  // Puts the event at `position` in `place`, nothing for a step with no event
  def moved(position: Option[Long], place: EventPlace): Pipeline =
    position.fold(this)(pos => copy(events = events.updated(pos, place)))

  // Load `loadNumber` is launched: it takes the event at `position`, and
  // keeps the events batched for it
  def handleLoad(position: Long, loadNumber: Int): Pipeline =
    val load = loadOrPending(loadNumber)
    Pipeline(
      events - position,
      loads.updated(
        loadNumber,
        Load(LoadPlace.InFlight, load.events + position)
      )
    )

  // The event at `position` waits for load `servedBy`, not launched yet
  def handleBatched(position: Long, servedBy: Int): Pipeline =
    val load = loadOrPending(servedBy)
    Pipeline(
      events - position,
      loads.updated(servedBy, load.copy(events = load.events + position))
    )

  def handleAnswer(loadNumber: Int, position: Long): Pipeline =
    withLoadPlace(loadNumber, LoadPlace.Answered(position))

  def handleError(loadNumber: Int): Pipeline =
    withLoadPlace(loadNumber, LoadPlace.Failed)

  // A value loaded from scratch (first open, key change, reconnect): the loads
  // in flight are cancelled, the events not yet in a load are dropped, and
  // load `loadNumber` keeps the events batched for it
  def handleInitialLoad(loadNumber: Int): Pipeline =
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
    loads.values.exists(_.place match
      case LoadPlace.Answered(_) => true
      case _                     => false)

  // The loads answered are put on screen
  def displayAnswered(): Pipeline =
    copy(loads =
      loads.map((number, load) =>
        load.place match
          case LoadPlace.Answered(_) =>
            (number, load.copy(place = LoadPlace.Displayed))
          case _ => (number, load)
      )
    )

  // What a step left behind is removed on the next one: an event not
  // considered, and a load done, with its events. A load answered stays until
  // it is displayed
  def automaticStep(): Pipeline =
    Pipeline(
      events.filter((_, place) => place != EventPlace.NotConsidered),
      loads.filter((_, load) =>
        load.place match
          case LoadPlace.Pending | LoadPlace.InFlight | LoadPlace.Answered(_) =>
            true
          case LoadPlace.Displayed | LoadPlace.Failed | LoadPlace.Cancelled =>
            false
      )
    )

  // A load not seen yet (batched for, or started before the stepper) is
  // pending with no event
  private def loadOrPending(loadNumber: Int): Load =
    loads.getOrElse(loadNumber, Load(LoadPlace.Pending, Set.empty))

  private def withLoadPlace(loadNumber: Int, place: LoadPlace): Pipeline =
    copy(loads =
      loads.updated(loadNumber, loadOrPending(loadNumber).copy(place = place))
    )

object Pipeline:
  val empty: Pipeline = Pipeline(Map.empty, Map.empty)

// `source` is the name of the live data this stepper follows, the same as the
// `name` of its `RefreshedLiveData`: its loads are numbered per live data
case class RefreshStepperState(
    source: String,
    currentStep: Option[TraceStep],
    pipeline: Pipeline,
    history: List[TraceStep]
):
  def hasNextStep(): Boolean =
    history.exists(concerns) || pipeline.hasAnswered

  def step(): RefreshStepperState =
    if !hasNextStep() then this
    // A load answered is displayed on a step of its own, before the next
    // trace step: it is the one that changes the screen. No trace step is
    // behind it, hence no current step
    else if pipeline.hasAnswered then
      copy(
        currentStep = None,
        pipeline = pipeline.automaticStep().displayAnswered()
      )
    else
      // The steps of other live data are skipped, they are not shown here
      val remaining = history.dropWhile(traceStep => !concerns(traceStep))
      val consideredStep = remaining.head
      val newHistory = remaining.tail
      val newPipeline = updatedPipeline(pipeline, consideredStep)

      RefreshStepperState(
        source,
        Some(consideredStep),
        newPipeline,
        newHistory
      )

  // Whether a step is one of this stepper's: the steps every event goes
  // through, and the ones of its own source
  private def concerns(traceStep: TraceStep): Boolean =
    traceStep.kind match
      case Kind.Received | Kind.Ended     => true
      case Kind.Considered(stepSource)    => stepSource == source
      case Kind.NotConsidered(stepSource) => stepSource == source
      case Kind.Reload(stepSource, _)     => stepSource == source
      case Kind.Decoded | Kind.Folded(_)  => false
      case Kind.FiguresComputed(_)        => false

  private def updatedPipeline(
      pipeline: Pipeline,
      consideredStep: TraceStep
  ): Pipeline =
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
          case Outcome.Answer(pos, loadNumber) =>
            cleaned.handleAnswer(loadNumber, pos)
          case Outcome.AnswerAndLaunchPending(answer, load) =>
            cleaned
              .handleAnswer(answer.loadNumber, answer.position)
              .handleLoad(load.position, load.loadNumber)
          case Outcome.Error(loadNumber) =>
            cleaned.handleError(loadNumber)
          case Outcome.ErrorAndLaunchPending(error, load) =>
            cleaned
              .handleError(error.loadNumber)
              .handleLoad(load.position, load.loadNumber)
      case _ => cleaned
