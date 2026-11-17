package prototype.client.views.dashboard

import prototype.client.live.{LiveData, ReplayedStepper, TraceStep, Tracer}
import prototype.client.live.ReplayedStepper.{EventPlace, Shown, SnapshotPlace}
import prototype.client.views.components.{StepCard, StepGraph}

import com.raquo.laminar.api.L.*

object BrowserStepByStep:

  /** The step-by-step mode of a dashboard on the browser level: its live data
    * computed in the browser, held by a `ReplayedStepper` that keeps only the
    * steps of `sources` (the replayed live data it is computed from), drawn on
    * the browser's graph.
    */
  def apply[A](
      tracer: Tracer,
      source: String,
      sources: Set[String],
      live: LiveData[A]
  ): StepByStep[A] =
    StepByStepView[ReplayedStepper[A], A](
      tracer,
      source,
      live,
      start = real =>
        ReplayedStepper.empty[A](source, sources).copy(displayed = real),
      // A recorded step only grows `history`, which the panel does not draw
      drawn = stepper => (stepper.currentStep, stepper.pipeline),
      renderCurrentStep = renderCurrentStep,
      renderGraph = renderGraph
    )

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
          case (TraceStep.Kind.SnapshotRequested(source, _), _) =>
            val dropped =
              if pipeline.cancelled then
                "; the browser dropped the one still loading"
              else ""
            ("", s"Snapshot of $source requested from the server$dropped")
          case (TraceStep.Kind.SnapshotLoaded(source, position, _), _) =>
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

    // What was clicked, whose card is open: drawn again on each Next, so a
    // card closes when the panel steps
    val openedVar = Var(Option.empty[Opened])

    // What opens an event's card: its dot, or the figures it led to
    def clickableEvent(position: Long)(content: HtmlElement): HtmlElement =
      StepCard.clickable(
        s"step-event-$position",
        s"Details of event $position",
        () => openedVar.set(Some(Opened.Event(position)))
      )(content)

    def event(position: Long): HtmlElement =
      clickableEvent(position)(StepGraph.event(position))

    def figures(onScreen: Boolean): HtmlElement =
      pipeline.currentEvent.fold(renderFigures(None, onScreen, "no event"))(
        position =>
          clickableEvent(position)(
            renderFigures(Some(position), onScreen, "")
          )
      )

    def snapshot(place: SnapshotPlace): HtmlElement =
      StepCard.clickable(
        "step-snapshot",
        "Details of the snapshot",
        () => openedVar.set(Some(Opened.Snapshot))
      )(renderSnapshot(place))

    // The event shown, in the node of its place
    def eventIn(which: Node): List[HtmlElement] =
      if !eventNode.contains(which) then Nil
      else
        place match
          case Some(EventPlace.FiguresComputed) => List(figures(false))
          case Some(EventPlace.FromSnapshot) =>
            List(renderFigures(None, false, "from the snapshot"))
          case Some(EventPlace.Recomputed) =>
            List(renderFigures(None, false, "no event"))
          case Some(EventPlace.Displayed) => List(figures(true))
          case _ => pipeline.currentEvent.map(event).toList

    def snapshotIn(test: SnapshotPlace => Boolean): List[HtmlElement] =
      pipeline.snapshot.filter(test).map(snapshot).toList

    val waiting = pipeline.waiting.toList.sorted.map(event)
    val folded = eventIn(Node.Folded) ++ snapshotIn {
      case SnapshotPlace.Received(_) | SnapshotPlace.FoldedOnTop(_) => true
      case _                                                        => false
    }
    val keptAside = eventIn(Node.KeptAside) ++ waiting
    val failed = snapshotIn(_ == SnapshotPlace.Failed)
    val ignored = eventIn(Node.Ignored)

    def node(which: Node, title: String, body: List[HtmlElement]) =
      StepGraph.node(nodeClass(which), title, now.contains(which), body)

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
        node(Node.OnScreen, "On screen", eventIn(Node.OnScreen)),
        StepCard.popoverCard(
          openedVar.signal.map(_.map(anchorOf)),
          openedVar.signal.map(_.map(card(stepper, _)))
        )
      )
    )

  // What a click opened a card on
  private enum Opened:
    case Event(position: Long)
    case Snapshot

  private def anchorOf(opened: Opened): String =
    opened match
      case Opened.Event(position) => s"step-event-$position"
      case Opened.Snapshot        => "step-snapshot"

  // The card of what was clicked, from what the stepper kept about it
  private def card(stepper: ReplayedStepper[?], opened: Opened): HtmlElement =
    opened match
      case Opened.Event(position) =>
        stepper.eventCards.get(position) match
          case None =>
            StepCard.frame(
              s"Event $position",
              StepCard.facts("Arrived" -> "before the panel opened")
            )
          case Some(event) =>
            StepCard.frame(
              s"Event $position · ${event.eventType}",
              StepCard.steps(event.steps),
              StepCard.payload(event.payload)
            )
      case Opened.Snapshot =>
        stepper.snapshotCard.fold(StepCard.frame("Snapshot"))(snapshot =>
            val size =
              if snapshot.receivedAt.isEmpty then "not received yet"
              else snapshot.size.fold("received")(items => s"$items items")
            val folded = stepper.pipeline.snapshot match
              case Some(SnapshotPlace.FoldedOnTop(events)) =>
                if events.isEmpty then "none"
                else events.toList.sorted.mkString(", ")
              case _ =>
                if stepper.pipeline.waiting.isEmpty then "none waiting"
                else
                  "waiting: " +
                    stepper.pipeline.waiting.toList.sorted.mkString(", ")
            StepCard.frame(
              s"Snapshot of ${snapshot.source}",
              StepCard.facts(
                "Request" -> snapshot.request,
                "Load" -> snapshot.requestedAt.fold("—")(at =>
                  StepCard.between(at, snapshot.receivedAt, "loading")
                ),
                "Size" -> size,
                "Events folded on top" -> folded
              )
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
    StepGraph.arrows(
      width = 910,
      height = 326,
      idPrefix = "browser",
      usual = usual,
      occasional = List(Move.FromStream, Move.FromBrowser, Move.Recomputed),
      lit = lit,
      path = movePath,
      label = moveLabel
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

  // The figures: a capsule like a reload's on the server, holding the event
  // they come from (or a note when none), filled once on screen
  private def renderFigures(
      position: Option[Long],
      onScreen: Boolean,
      note: String
  ): HtmlElement =
    StepGraph.capsule(
      if onScreen then "displayed" else "answered",
      "Figures",
      position.toList,
      Option.when(position.isEmpty && note.nonEmpty)(note)
    )

  // The snapshot: a capsule like a reload's, holding the events folded on top
  // of it once they are
  private def renderSnapshot(place: SnapshotPlace): HtmlElement =
    place match
      case SnapshotPlace.InFlight =>
        StepGraph.capsule("in-flight", "Snapshot", Nil, Some("loading"))
      case SnapshotPlace.Received(position) =>
        StepGraph.capsule("answered", "Snapshot", Nil, Some(s"up to $position"))
      case SnapshotPlace.FoldedOnTop(events) =>
        StepGraph.capsule("answered", "Snapshot", events.toList, None)
      case SnapshotPlace.Failed =>
        StepGraph.capsule("failed", "Snapshot", Nil, Some("failed"))

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
          case TraceStep.Kind.Received(_, _) =>
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
          case TraceStep.Kind.SnapshotRequested(_, _) =>
            (Set(Node.ServerLoading), Set(Move.FromBrowser))
          case TraceStep.Kind.SnapshotLoaded(_, _, _) =>
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
