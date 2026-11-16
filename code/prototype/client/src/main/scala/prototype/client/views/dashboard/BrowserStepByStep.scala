package prototype.client.views.dashboard

import prototype.client.live.{LiveData, ReplayedStepper, TraceStep, Tracer}
import prototype.client.live.ReplayedStepper.{EventPlace, Shown, SnapshotPlace}
import prototype.client.views.components.PopoverPicker
import prototype.client.views.components.PopoverPicker.Item

import com.raquo.laminar.api.L.*

final class BrowserStepByStep[A](
    tracer: Tracer,
    source: String,
    sources: Set[String],
    val live: LiveData[A]
) extends StepByStep[A]:
  import BrowserStepByStep.*

  // The time between two steps while playing, in milliseconds
  private val speedVar: Var[Int] = Var(1000)
  private val playingVar: Var[Boolean] = Var(false)
  // Off: Next skips the events this dashboard ignores
  private val showIgnoredVar: Var[Boolean] = Var(false)

  private val newSteps: EventStream[Action] =
    tracer.steps.map(Action.AddStep(_))
  private val pressedNextBus: EventBus[Action] = new EventBus
  private val pressedLatestBus: EventBus[Action] = new EventBus
  private val playBus: EventStream[Action] =
    speedVar.signal
      .combineWith(playingVar.signal)
      .changes
      .flatMapSwitch((speed, play) =>
        if play then EventStream.periodic(speed).mapTo(Action.Next)
        else EventStream.empty
      )

  private val nextS = pressedNextBus.events.mergeWith(playBus)

  private val stepper: Signal[ReplayedStepper[A]] =
    newSteps
      .mergeWith(nextS, pressedLatestBus.events)
      .mergeWith(EventStream.fromValue(Action.Reset)) // Start with a reset
      .withCurrentValueOf(showIgnoredVar.signal, live.valueS)
      .scanLeft(ReplayedStepper.empty[A](source, sources)) {
        case (stepper, (action, showIgnored, real)) =>
          action match
            case Action.AddStep(traceStep) => stepper.addStep(traceStep)
            case Action.Next  => stepper.step(skipIgnored = !showIgnored)
            case Action.Reset =>
              ReplayedStepper.empty[A](source, sources).copy(displayed = real)
      }

  // The stepper as the panel draws it: a recorded step only grows
  // `history`, which the current step and the graph do not show
  private val shown: Signal[ReplayedStepper[A]] =
    stepper.distinctBy(s => (s.currentStep, s.pipeline))

  val steppedData: LiveData[A] = new LiveData[A]:
    val valueS: Signal[Option[A]] = stepper.map(_.displayed)
    val errorS: Signal[Option[Throwable]] = Val(None)
    val loadingS: Signal[Boolean] = Val(false)

  def panel: HtmlElement =
    div(
      cls := "stepper",
      role := "region",
      aria.label := "Step by step",
      div(
        cls := "stepper__bar",
        div(
          cls := "stepper__heading",
          span(cls := "stepper__source", source),
          span(
            cls := "stepper__left",
            text <-- stepper
              .combineWith(showIgnoredVar.signal)
              .map((s, showIgnored) => stepsLeft(s.stepsLeft(!showIgnored)))
          )
        ),
        renderControls(
          stepper,
          playingVar,
          speedVar,
          showIgnoredVar,
          pressedLatestBus.writer,
          pressedNextBus.writer
        )
      ),
      // Drawn again only when what they show changes (a Next), not on every
      // step recorded: during a burst thousands are recorded per second
      // Read out by screen readers at each Next, but not while playing
      div(
        aria.live <-- playingVar.signal.map(playing =>
          if playing then "off" else "polite"
        ),
        child <-- shown.map(renderCurrentStep)
      ),
      child <-- shown.map(renderGraph)
    )

object BrowserStepByStep:

  // The moves an arrow can show, from one node to the next, or from outside
  // the graph (the event stream, the browser asking for a snapshot, a period
  // change)
  private enum Move:
    case FromStream, ToIgnored, ToDecoded, ToFolded, ToKeptAside, FromBrowser,
      ToFailed, ToSnapshot, FoldOnTop, ToComputed, Recomputed, ToScreen

  // The nodes of the graph: the event's places on the browser's lane, and the
  // snapshot's on the server's. A snapshot dropped for a new one has no node:
  // the browser drops it, the server knows nothing of it
  private enum Node:
    case Received, Ignored, Decoded, Folded, KeptAside, Computed, OnScreen,
      ServerLoading, Failed

  private enum Action:
    case AddStep(step: TraceStep)
    case Next
    case Reset

  // The controls: Jump to latest (starts the stepper again from now),
  // Play / Pause, the speed, Show ignored, Next step (off while playing)
  private def renderControls(
      stepperS: Signal[ReplayedStepper[?]],
      playingVar: Var[Boolean],
      speedVar: Var[Int],
      showIgnoredVar: Var[Boolean],
      pressedLatest: Observer[Action],
      pressedNext: Observer[Action]
  ): HtmlElement =
    div(
      cls := "stepper__controls",
      renderJumpButton(pressedLatest),
      renderPlayButton(playingVar),
      renderSpeedSelector(speedVar),
      renderShowIgnoredToggle(showIgnoredVar),
      renderNextButton(
        stepperS,
        playingVar.signal,
        showIgnoredVar.signal,
        pressedNext
      )
    )

  private def renderJumpButton(pressedLatest: Observer[Action]): HtmlElement =
    button(
      tpe := "button",
      cls := "btn stepper__jump",
      "Jump to latest",
      onClick.mapTo(Action.Reset) --> pressedLatest
    )

  // Play while paused, Pause while playing
  private def renderPlayButton(playingVar: Var[Boolean]): HtmlElement =
    button(
      tpe := "button",
      cls := "btn stepper__play",
      cls("stepper__play--on") <-- playingVar.signal,
      aria.label <-- playingVar.signal.map(playing =>
        if playing then "Pause" else "Play"
      ),
      child <-- playingVar.signal.map(playIcon),
      onClick --> (_ => playingVar.update(!_))
    )

  // The time between two steps while playing
  private def renderSpeedSelector(speedVar: Var[Int]): HtmlElement =
    PopoverPicker(
      id = "step-speed-selector",
      label = "Every",
      items = Val(
        List(
          Item(50, "0.05 s"),
          Item(250, "0.25 s"),
          Item(500, "0.5 s"),
          Item(1000, "1 s"),
          Item(2000, "2 s"),
          Item(3000, "3 s")
        )
      ),
      selectedVar = speedVar
    )

  // On: every step, the events this dashboard ignores too
  private def renderShowIgnoredToggle(
      showIgnoredVar: Var[Boolean]
  ): HtmlElement =
    button(
      tpe := "button",
      cls := "btn stepper__toggle",
      cls("stepper__toggle--on") <-- showIgnoredVar.signal,
      aria.pressed <-- showIgnoredVar.signal.map(_.toString),
      title := "Also step through the events this dashboard ignores",
      span(cls := "stepper__switch", aria.hidden := true),
      "Show ignored",
      onClick --> (_ => showIgnoredVar.update(!_))
    )

  // Off while playing, or when no step is left
  private def renderNextButton(
      stepperS: Signal[ReplayedStepper[?]],
      playingS: Signal[Boolean],
      showIgnoredS: Signal[Boolean],
      pressedNext: Observer[Action]
  ): HtmlElement =
    button(
      tpe := "button",
      cls := "btn btn--primary stepper__next",
      disabled <-- stepperS
        .combineWith(playingS, showIgnoredS)
        .map((stepper, play, showIgnored) =>
          play || !stepper.hasNextStep(!showIgnored)
        ),
      "Next step",
      span(aria.hidden := true, "→"),
      onClick.mapTo(Action.Next) --> pressedNext
    )

  // A triangle to play, two bars to pause
  private def playIcon(playing: Boolean): SvgElement =
    svg.svg(
      svg.width := "14",
      svg.height := "14",
      svg.viewBox := "0 0 14 14",
      svg.fill := "none",
      aria.hidden := true,
      svg.path(
        svg.d := (if playing then "M4.5 2.5v9 M9.5 2.5v9"
                  else "M3.5 2.5v9l8-4.5z")
      )
    )

  // The step just shown, as one line
  private def renderCurrentStep(stepper: ReplayedStepper[?]): HtmlElement =
    val pipeline = stepper.pipeline
    val (modifier, line) = stepper.currentStep match
      case Shown.Idle    => ("none", "Waiting for the next step")
      case Shown.Display => ("shown", "The figures are on screen")
      case Shown.FoldedOnTop(applied, alreadyIn) =>
        val alreadyInText =
          if alreadyIn == 0 then ""
          else s", ${events(alreadyIn)} already in it"
        ("", s"${events(applied)} folded on top of the snapshot$alreadyInText")
      case Shown.Trace(step) =>
        (step.kind, pipeline.currentPlace) match
          case (_, Some(EventPlace.Ignored)) =>
            ("", "Received, not for this dashboard")
          case (TraceStep.Kind.Folded(_, false), _) =>
            if pipeline.snapshot.contains(SnapshotPlace.InFlight) then
              ("", "Kept aside until the snapshot arrives")
            else ("", "Kept aside: already in the snapshot")
          case (_, Some(EventPlace.FromSnapshot)) =>
            ("", "Figures computed from the snapshot")
          case (_, Some(EventPlace.Recomputed)) =>
            ("", "Figures computed again, with no event behind them")
          case (TraceStep.Kind.SnapshotRequested(source), _) =>
            val dropped =
              if pipeline.cancelled then
                "; the browser dropped the one still loading"
              else ""
            ("", s"Snapshot of $source requested from the server$dropped")
          case (TraceStep.Kind.SnapshotLoaded(source, position), _) =>
            ("", s"Snapshot of $source received, up to event $position")
          case (TraceStep.Kind.SnapshotFailed(source), _) =>
            ("", s"Snapshot of $source failed")
          case (TraceStep.Kind.Ended, _) => ("none", "The event is done")
          case _                         => ("", TraceStep.describe(step))
    div(
      cls := "stepper__current",
      span(cls := "stepper__label", "Current step"),
      span(
        cls := "stepper__step" +
          (if modifier.isEmpty then "" else s" stepper__step--$modifier"),
        line
      )
    )

  private def events(count: Int): String =
    if count == 1 then "1 event" else s"$count events"

  // The browser's lane, the event going from node to node one at a time, and
  // above it the server's, where a snapshot loads. Ignored, Kept aside,
  // and Failed only show when something is in them
  private def renderGraph(stepper: ReplayedStepper[?]): HtmlElement =
    val pipeline = stepper.pipeline
    val place = pipeline.currentPlace
    val (now, lit) = highlighted(stepper)
    val eventNode = place.map(nodeOf)

    // The event shown, in the node of its place
    def eventIn(which: Node): List[HtmlElement] =
      if !eventNode.contains(which) then Nil
      else
        place match
          case Some(EventPlace.FiguresComputed) =>
            List(renderFigures(pipeline.currentEvent, false, "no event"))
          case Some(EventPlace.FromSnapshot) =>
            List(renderFigures(None, false, "from the snapshot"))
          case Some(EventPlace.Recomputed) =>
            List(renderFigures(None, false, "no event"))
          case Some(EventPlace.Displayed) =>
            List(renderFigures(pipeline.currentEvent, true, ""))
          case _ => pipeline.currentEvent.map(renderEvent).toList

    def snapshotIn(test: SnapshotPlace => Boolean): List[HtmlElement] =
      pipeline.snapshot.filter(test).map(renderSnapshot).toList

    val waiting = pipeline.waiting.toList.sorted.map(renderEvent)
    val folded = eventIn(Node.Folded) ++ snapshotIn {
      case SnapshotPlace.Received(_) | SnapshotPlace.FoldedOnTop(_) => true
      case _                                                        => false
    }
    val keptAside = eventIn(Node.KeptAside) ++ waiting
    val failed = snapshotIn(_ == SnapshotPlace.Failed)
    val ignored = eventIn(Node.Ignored)

    def node(which: Node, title: String, body: List[HtmlElement]) =
      renderNode(which, title, now.contains(which), body)

    div(
      cls := "stepper__graph-scroll",
      div(
        cls := "stepper__graph stepper__graph--browser",
        span(
          cls := "stepper__label stepper__lane stepper__lane--server",
          "Server"
        ),
        span(
          cls := "stepper__label stepper__lane stepper__lane--browser",
          "Browser"
        ),
        renderArrows(
          lit,
          ignored.nonEmpty,
          keptAside.nonEmpty,
          failed.nonEmpty
        ),
        node(Node.Received, "Arrived", eventIn(Node.Received)),
        Option.when(ignored.nonEmpty)(
          node(Node.Ignored, "Not for this dashboard", ignored)
        ),
        node(Node.Decoded, "Decoded", eventIn(Node.Decoded)),
        node(Node.Folded, "Folded into the lists", folded),
        Option.when(keptAside.nonEmpty)(
          node(Node.KeptAside, "Kept aside", keptAside)
        ),
        node(
          Node.ServerLoading,
          "Server loading",
          snapshotIn(_ == SnapshotPlace.InFlight)
        ),
        Option.when(failed.nonEmpty)(
          node(Node.Failed, "Snapshot failed", failed)
        ),
        node(Node.Computed, "Figures computed", eventIn(Node.Computed)),
        node(Node.OnScreen, "On screen", eventIn(Node.OnScreen))
      )
    )

  private def nodeOf(place: EventPlace): Node =
    place match
      case EventPlace.Received        => Node.Received
      case EventPlace.Ignored         => Node.Ignored
      case EventPlace.Decoded         => Node.Decoded
      case EventPlace.Folded          => Node.Folded
      case EventPlace.KeptAside       => Node.KeptAside
      case EventPlace.FiguresComputed => Node.Computed
      case EventPlace.FromSnapshot    => Node.Computed
      case EventPlace.Recomputed      => Node.Computed
      case EventPlace.Displayed       => Node.OnScreen

  private def renderNode(
      which: Node,
      title: String,
      isNow: Boolean,
      body: Seq[HtmlElement]
  ): HtmlElement =
    div(
      cls := s"stepper__node stepper__node--${nodeClass(which)}" +
        (if isNow then " stepper__node--now" else ""),
      div(
        cls := "stepper__node-head",
        span(cls := "stepper__node-title", title),
        Option.when(isNow)(span(cls := "stepper__now", "now"))
      ),
      div(cls := "stepper__node-body", body)
    )

  // The arrows, in the graph's own coordinates (see stepper.css, the browser
  // graph). The usual moves are always drawn, except towards a hidden node;
  // the ones from outside (the event stream, the browser asking for a
  // snapshot, a period change) only during the step that makes them. The
  // moves of the current step are amber, drawn last so that they sit on top
  private def renderArrows(
      lit: Set[Move],
      ignored: Boolean,
      keptAside: Boolean,
      failed: Boolean
  ): SvgElement =
    val usual = List(
      Option.when(ignored)(Move.ToIgnored),
      Some(Move.ToDecoded),
      Some(Move.ToFolded),
      Option.when(keptAside)(Move.ToKeptAside),
      Option.when(keptAside)(Move.FoldOnTop),
      Option.when(failed)(Move.ToFailed),
      Some(Move.ToSnapshot),
      Some(Move.ToComputed),
      Some(Move.ToScreen)
    ).flatten
    val occasional =
      List(Move.FromStream, Move.FromBrowser, Move.Recomputed)
        .filter(lit.contains)
    val (now, others) = (usual ++ occasional).partition(lit.contains)

    def marker(id: String, modifier: String) =
      svg.marker(
        svg.idAttr := id,
        svg.viewBox := "0 0 8 8",
        svg.refX := "7",
        svg.refY := "4",
        svg.markerWidth := "8",
        svg.markerHeight := "8",
        svg.orient := "auto",
        svg.path(svg.d := "M0 0 8 4 0 8z", svg.cls := modifier)
      )

    def arrow(move: Move, isNow: Boolean) =
      svg.path(
        svg.cls := (if isNow then "stepper__arrow stepper__arrow--now"
                    else "stepper__arrow"),
        svg.d := movePath(move),
        svg.markerEnd := (if isNow then "url(#browser-arrow-head-now)"
                          else "url(#browser-arrow-head)")
      )

    svg.svg(
      svg.cls := "stepper__arrows",
      svg.width := "910",
      svg.height := "326",
      svg.viewBox := "0 0 910 326",
      svg.fill := "none",
      aria.hidden := true,
      svg.defs(
        marker("browser-arrow-head", "stepper__arrow-head"),
        marker(
          "browser-arrow-head-now",
          "stepper__arrow-head stepper__arrow-head--now"
        )
      ),
      others.map(arrow(_, false)),
      now.map(arrow(_, true)),
      now
        .flatMap(moveLabel)
        .map((label, x, y) =>
          svg.text(
            svg.cls := "stepper__arrow-label",
            svg.x := x.toString,
            svg.y := y.toString,
            label
          )
        )
    )

  private def movePath(move: Move): String =
    move match
      case Move.FromStream  => "M147 34 L147 118"
      case Move.ToIgnored   => "M147 204 L147 248"
      case Move.ToDecoded   => "M222 162 L242 162"
      case Move.ToFolded    => "M394 162 L414 162"
      case Move.ToKeptAside => "M319 204 L319 284 L414 284"
      case Move.FoldOnTop   => "M491 250 L491 206"
      case Move.FromBrowser => "M262 52 L414 52"
      case Move.ToFailed    => "M566 52 L586 52"
      case Move.ToSnapshot  => "M491 94 L491 118"
      case Move.ToComputed  => "M566 162 L586 162"
      case Move.Recomputed  => "M663 290 L663 206"
      case Move.ToScreen    => "M738 162 L758 162"

  // Where an occasional arrow comes from, written at its start
  private def moveLabel(move: Move): Option[(String, Int, Int)] =
    move match
      case Move.FromStream  => Some(("event stream", 147, 28))
      case Move.FromBrowser => Some(("opened or changed", 316, 44))
      case Move.Recomputed  => Some(("period changed", 663, 306))
      case _                => None

  // The event: its dot and its position
  private def renderEvent(position: Long): HtmlElement =
    span(
      cls := "stepper__event",
      renderDot(position),
      span(cls := "stepper__position", aria.hidden := true, position.toString)
    )

  // One colour per event, from its position
  private def renderDot(position: Long): HtmlElement =
    span(
      cls := s"stepper__dot stepper__dot--${position % 8}",
      role := "img",
      aria.label := s"event $position",
      title := s"event $position"
    )

  // The figures: a capsule like a reload's on the server, holding the event
  // they come from (or a note when none), filled once on screen
  private def renderFigures(
      position: Option[Long],
      onScreen: Boolean,
      note: String
  ): HtmlElement =
    span(
      cls := "stepper__load " +
        (if onScreen then "stepper__load--displayed"
         else "stepper__load--answered"),
      span(cls := "stepper__load-name", "Figures"),
      position match
        case Some(pos) => renderDot(pos)
        case None      =>
          Option.when(note.nonEmpty)(span(cls := "stepper__load-note", note))
    )

  // The snapshot: a capsule like a reload's, holding the events folded on top
  // of it once they are
  private def renderSnapshot(place: SnapshotPlace): HtmlElement =
    place match
      case SnapshotPlace.InFlight =>
        renderSnapshotNote("in-flight", "loading")
      case SnapshotPlace.Received(position) =>
        renderSnapshotNote("answered", s"up to $position")
      case SnapshotPlace.FoldedOnTop(events) =>
        span(
          cls := "stepper__load stepper__load--answered",
          span(cls := "stepper__load-name", "Snapshot"),
          events.toList.sorted.map(renderDot)
        )
      case SnapshotPlace.Failed => renderSnapshotNote("failed", "failed")

  private def renderSnapshotNote(modifier: String, note: String): HtmlElement =
    span(
      cls := s"stepper__load stepper__load--$modifier",
      span(cls := "stepper__load-name", "Snapshot"),
      span(cls := "stepper__load-note", note)
    )

  // What the current step changed, drawn in amber: the nodes it put
  // something in, and the moves it made
  private def highlighted(
      stepper: ReplayedStepper[?]
  ): (Set[Node], Set[Move]) =
    val pipeline = stepper.pipeline
    stepper.currentStep match
      case Shown.Idle               => (Set.empty, Set.empty)
      case Shown.Display            => (Set(Node.OnScreen), Set(Move.ToScreen))
      case Shown.FoldedOnTop(_, _)  => (Set(Node.Folded), Set(Move.FoldOnTop))
      case Shown.Trace(step) =>
        step.kind match
          case TraceStep.Kind.Received =>
            if pipeline.currentPlace.contains(EventPlace.Ignored) then
              (Set(Node.Ignored), Set(Move.FromStream, Move.ToIgnored))
            else (Set(Node.Received), Set(Move.FromStream))
          case TraceStep.Kind.Decoded(_) =>
            (Set(Node.Decoded), Set(Move.ToDecoded))
          case TraceStep.Kind.Folded(_, true) =>
            (Set(Node.Folded), Set(Move.ToFolded))
          case TraceStep.Kind.Folded(_, false) =>
            (Set(Node.KeptAside), Set(Move.ToKeptAside))
          case TraceStep.Kind.FiguresComputed(_, _) =>
            if pipeline.currentPlace.contains(EventPlace.Recomputed) then
              (Set(Node.Computed), Set(Move.Recomputed))
            else (Set(Node.Computed), Set(Move.ToComputed))
          case TraceStep.Kind.SnapshotRequested(_) =>
            (Set(Node.ServerLoading), Set(Move.FromBrowser))
          case TraceStep.Kind.SnapshotLoaded(_, _) =>
            (Set(Node.Folded), Set(Move.ToSnapshot))
          case TraceStep.Kind.SnapshotFailed(_) =>
            (Set(Node.Failed), Set(Move.ToFailed))
          case _ => (Set.empty, Set.empty)

  private def nodeClass(which: Node): String =
    which match
      case Node.Received      => "received"
      case Node.Ignored       => "ignored"
      case Node.Decoded       => "decoded"
      case Node.Folded        => "folded"
      case Node.KeptAside     => "kept-aside"
      case Node.Computed      => "computed"
      case Node.OnScreen      => "on-screen"
      case Node.ServerLoading => "server"
      case Node.Failed        => "failed"

  private def stepsLeft(count: Int): String =
    count match
      case 0 => "no step left"
      case 1 => "1 step left"
      case n => s"$n steps left"
