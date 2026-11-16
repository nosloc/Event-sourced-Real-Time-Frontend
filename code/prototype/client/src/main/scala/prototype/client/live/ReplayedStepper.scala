package prototype.client.live

import prototype.client.live.TraceStep.Kind

/** @param source
  *   the name of the dashboard this stepper follows, the one its figures are
  *   computed with
  * @param sources
  *   the names of the replayed live data the dashboard is computed from: only
  *   their steps are shown
  * @param pipeline
  *   where the event shown and the snapshot are, and the events waiting for it
  * @param currentStep
  *   what the last Next showed
  * @param history
  *   the entries not shown yet, in the order recorded, with the ignored events
  * @param displayed
  *   the figures on screen
  * @param ignoredCount
  *   the number of ignored events in `history`, so that what is left to show is
  *   known without reading the history
  */
final case class ReplayedStepper[+A](
    source: String,
    sources: Set[String],
    pipeline: ReplayedStepper.Pipeline,
    currentStep: ReplayedStepper.Shown,
    history: Vector[ReplayedStepper.Entry[A]],
    displayed: Option[A],
    ignoredCount: Int = 0
) extends Stepper[ReplayedStepper[A], A]:
  import ReplayedStepper.*

  override def hasNextStep(skipIgnored: Boolean): Boolean =
    stepsLeft(skipIgnored) > 0 || pipeline.hasAutomaticStepPending

  override def stepsLeft(skipIgnored: Boolean): Int =
    if skipIgnored then history.length - ignoredCount
    else history.length

  override def next(skipIgnored: Boolean): ReplayedStepper[A] =
    if !hasNextStep(skipIgnored) then this
    else
      // When skipped, the ignored events at the head leave the history at
      // once, without a recursion per event: a burst can hold thousands
      val remaining =
        if skipIgnored then
          history.dropWhile {
            case Entry.Ignored(_, _) => true
            case _                   => false
          }
        else history
      val count = ignoredCount - (history.size - remaining.size)
      remaining.headOption match
        case None =>
          this.copy(
            pipeline = pipeline.automaticStep(),
            history = remaining,
            ignoredCount = count,
            currentStep = Shown.Idle
          )
        // Only reached when the ignored events are shown
        case Some(Entry.Ignored(received, _)) =>
          this.copy(
            pipeline = pipeline.cleaned.moved(
              received.position,
              EventPlace.Ignored
            ),
            history = remaining.tail,
            ignoredCount = count - 1,
            currentStep = Shown.Trace(received)
          )
        // Recurses once per event: the end empties the graph and the next
        // entry is shown on the same Next
        case Some(Entry.Step(step)) if step.kind == Kind.Ended =>
          this
            .copy(
              pipeline = pipeline.ended,
              history = remaining.tail,
              ignoredCount = count,
              currentStep = Shown.Trace(step)
            )
            .next(skipIgnored)
        // The figures computed are put on screen
        case Some(Entry.Display(value)) =>
          this.copy(
            pipeline = pipeline.cleaned.moved(
              pipeline.currentEvent,
              EventPlace.Displayed
            ),
            history = remaining.tail,
            ignoredCount = count,
            displayed = Some(value),
            currentStep = Shown.Display
          )
        // No event waited for the snapshot: nothing to fold on top, so no step
        case Some(Entry.FoldOnTop(_)) if pipeline.waiting.isEmpty =>
          this
            .copy(history = remaining.tail, ignoredCount = count)
            .next(skipIgnored)
        // The events kept aside during the load: the ones after the snapshot's
        // position are folded on top of it, the others are already in it
        case Some(Entry.FoldOnTop(position)) =>
          val (applied, alreadyIn) = pipeline.waiting.partition(_ > position)
          this.copy(
            pipeline = pipeline.foldedOnTop(applied),
            history = remaining.tail,
            ignoredCount = count,
            currentStep = Shown.FoldedOnTop(applied.size, alreadyIn.size)
          )
        case Some(Entry.Step(step)) =>
          this.copy(
            pipeline = updatedPipeline(pipeline, step),
            history = remaining.tail,
            ignoredCount = count,
            currentStep = Shown.Trace(step)
          )

  override def addStep(step: TraceStep): ReplayedStepper[A] =
    if concerns(step) then
      (step.kind, history.lastOption) match
        // An arrival right before its end: no live data of the dashboard
        // decoded it, the event is ignored
        case (Kind.Ended, Some(Entry.Step(prev)))
            if step.position == prev.position && prev.kind == Kind.Received =>
          this.copy(
            history = history.init :+ Entry.Ignored(prev, step),
            ignoredCount = ignoredCount + 1
          )
        // The one cast: safe because `concerns` only lets in the figures of
        // this stepper's dashboard
        case (Kind.FiguresComputed(_, figures), _) =>
          this.copy(
            history = history :+ Entry.Step(step) :+
              Entry.Display(figures.asInstanceOf[A])
          )
        // The events kept aside during the load are folded on top of the
        // snapshot on a step of their own, right after it arrived
        case (Kind.SnapshotLoaded(_, position), _) =>
          this.copy(
            history = history :+ Entry.Step(step) :+ Entry.FoldOnTop(position)
          )
        case _ =>
          this.copy(
            history = history :+ Entry.Step(step)
          )
    else this

  // Whether the stepper is concerned with the step: the arrival and the end
  // of every event, the steps of the dashboard's sources, and its figures
  private def concerns(traceStep: TraceStep): Boolean =
    traceStep.kind match
      case Kind.Received              => true
      case Kind.Ended                 => true
      case Kind.Decoded(s)            => sources.contains(s)
      case Kind.Folded(s, _)          => sources.contains(s)
      case Kind.SnapshotRequested(s)  => sources.contains(s)
      case Kind.SnapshotLoaded(s, _)  => sources.contains(s)
      case Kind.SnapshotFailed(s)     => sources.contains(s)
      case Kind.FiguresComputed(s, _) => s == source
      case Kind.Considered(_)         => false
      case Kind.NotConsidered(_)      => false
      case Kind.Reload(_, _)          => false

  private def updatedPipeline(pipeline: Pipeline, step: TraceStep): Pipeline =
    step.kind match
      // Read before the clean-up: what came before tells where the figures
      // come from. Right after a fold, from the event; right after a
      // snapshot, from it; otherwise they were computed again with no event
      // behind them (a period change)
      case Kind.FiguresComputed(_, _) =>
        if pipeline.currentPlace.exists(place =>
            place == EventPlace.Folded || place == EventPlace.KeptAside
          )
        then pipeline.cleaned.moved(step.position, EventPlace.FiguresComputed)
        else if pipeline.snapshot.exists {
            case SnapshotPlace.Received(_) | SnapshotPlace.FoldedOnTop(_) =>
              true
            case SnapshotPlace.InFlight | SnapshotPlace.Failed => false
          }
        then pipeline.cleaned.moved(None, EventPlace.FromSnapshot)
        else pipeline.cleaned.moved(None, EventPlace.Recomputed)
      case Kind.Received =>
        pipeline.cleaned.moved(step.position, EventPlace.Received)
      case Kind.Decoded(_) =>
        pipeline.cleaned.moved(step.position, EventPlace.Decoded)
      case Kind.Folded(_, applied) =>
        pipeline.cleaned.moved(
          step.position,
          if applied then EventPlace.Folded else EventPlace.KeptAside
        )
      case Kind.SnapshotRequested(_)        => pipeline.cleaned.requested
      case Kind.SnapshotLoaded(_, position) => pipeline.cleaned.loaded(position)
      case Kind.SnapshotFailed(_)           => pipeline.cleaned.failed
      // Not let in by `concerns`, or handled by `step`
      case _ =>
        pipeline

object ReplayedStepper:

  // Where the event shown is in the browser, one place for each node of the
  // panel's graph
  enum EventPlace:
    case Received
    // Shown with "Show ignored": no live data of the dashboard decoded it
    case Ignored
    case Decoded
    case Folded
    // Not folded: a snapshot is loading, or it already had the event
    case KeptAside
    case FiguresComputed
    // Figures computed from a snapshot just received
    case FromSnapshot
    // Figures computed with no event nor snapshot behind them
    case Recomputed
    case Displayed

  // Where the snapshot is: on the server, received, with the events folded on
  // top, or failed
  enum SnapshotPlace:
    case InFlight
    case Received(position: Long)
    case FoldedOnTop(events: Set[Long])
    case Failed

  /** @param currentEvent
    *   the event shown, if the step has one
    * @param currentPlace
    *   where the event (or the figures) shown is
    * @param snapshot
    *   the snapshot being loaded, or just received
    * @param waiting
    *   the events kept aside while the snapshot loads, to fold on top of it
    * @param cancelled
    *   a snapshot was dropped for a new one on the last step
    */
  final case class Pipeline(
      currentEvent: Option[Long],
      currentPlace: Option[EventPlace],
      snapshot: Option[SnapshotPlace],
      waiting: Set[Long],
      cancelled: Boolean
  ):

    def moved(position: Option[Long], place: EventPlace): Pipeline =
      this.copy(currentEvent = position, currentPlace = Some(place))

    // What a step left behind is removed on the next one: a snapshot done
    // (received, folded on top, failed) and a cancellation. A snapshot in
    // flight stays until it arrives
    def cleaned: Pipeline =
      this.copy(
        snapshot = snapshot.filter(_ == SnapshotPlace.InFlight),
        cancelled = false
      )

    // The event leaves the graph; kept aside while a snapshot loads, it waits
    // for it
    def ended: Pipeline =
      val keptForSnapshot =
        currentPlace.contains(EventPlace.KeptAside) &&
          snapshot.contains(SnapshotPlace.InFlight)
      this.copy(
        currentEvent = None,
        currentPlace = None,
        waiting = currentEvent
          .filter(_ => keptForSnapshot)
          .fold(waiting)(waiting + _)
      )

    // A new snapshot is asked for; one still in flight is dropped (the key
    // changed again). The events waiting stay: the new one folds them
    def requested: Pipeline =
      this.copy(
        snapshot = Some(SnapshotPlace.InFlight),
        cancelled = snapshot.contains(SnapshotPlace.InFlight)
      )

    def loaded(position: Long): Pipeline =
      this.copy(snapshot = Some(SnapshotPlace.Received(position)))

    // The events waiting stay for the next snapshot
    def failed: Pipeline =
      this.copy(snapshot = Some(SnapshotPlace.Failed))

    def foldedOnTop(applied: Set[Long]): Pipeline =
      this.copy(
        snapshot = Some(SnapshotPlace.FoldedOnTop(applied)),
        waiting = Set.empty,
        cancelled = false
      )

    // Whether the graph still shows something for the clean-up to remove: an
    // event ends on its `Ended`, but figures with no event behind them, a
    // snapshot done and a cancellation have none
    def hasAutomaticStepPending: Boolean =
      currentPlace.isDefined || cancelled ||
        snapshot.exists(_ != SnapshotPlace.InFlight)

    def automaticStep(): Pipeline =
      cleaned.copy(currentEvent = None, currentPlace = None)

  object Pipeline:
    def empty: Pipeline = Pipeline(None, None, None, Set.empty, false)

  // What the last Next showed: nothing yet (or only a clean-up), a trace
  // step, the figures put on screen, or the events folded on top of a
  // snapshot (how many, and how many it already had)
  enum Shown:
    case Idle
    case Trace(step: TraceStep)
    case Display
    case FoldedOnTop(applied: Int, alreadyIn: Int)

  // One entry of the history: a trace step, the figures to put on screen
  // after their step, an ignored event (its arrival and its end, merged), or
  // the events waiting to fold on top of the snapshot at `position`
  enum Entry[+A]:
    case Step(step: TraceStep)
    case Display(value: A)
    case Ignored(arrival: TraceStep, ended: TraceStep)
    case FoldOnTop(position: Long)

  def empty[A](source: String, sources: Set[String]): ReplayedStepper[A] =
    ReplayedStepper[A](
      source = source,
      sources = sources,
      pipeline = Pipeline.empty,
      currentStep = Shown.Idle,
      history = Vector.empty,
      displayed = None
    )
