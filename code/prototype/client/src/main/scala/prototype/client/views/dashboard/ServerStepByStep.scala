package prototype.client.views.dashboard

import prototype.client.live.{LiveData, RefreshStepper, TraceStep, Tracer}
import prototype.client.live.RefreshModel.Outcome
import prototype.client.live.RefreshStepper.{EventPlace, Load, LoadPlace, Shown}
import prototype.client.views.components.PopoverPicker
import prototype.client.views.components.PopoverPicker.Item

import com.raquo.laminar.api.L.*

final class ServerStepByStep[A](
    tracer: Tracer,
    source: String,
    val live: LiveData[A]
) extends StepByStep[A]:
  import ServerStepByStep.*

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

  private val stepper: Signal[RefreshStepper[A]] =
    newSteps
      .mergeWith(nextS, pressedLatestBus.events)
      .mergeWith(EventStream.fromValue(Action.Reset)) // Start with a reset
      .withCurrentValueOf(showIgnoredVar.signal, live.valueS)
      .scanLeft(RefreshStepper.empty[A](source)) {
        case (stepper, (action, showIgnored, real)) =>
          action match
            case Action.AddStep(traceStep) => stepper.addStep(traceStep)
            case Action.Next  => stepper.step(skipIgnored = !showIgnored)
            case Action.Reset =>
              RefreshStepper.empty[A](source).copy(displayed = real)
      }

  // The stepper as the panel draws it: a recorded step only grows
  // `history`, which the current step and the graph do not show
  private val shown: Signal[RefreshStepper[A]] =
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

object ServerStepByStep:

  // The moves an arrow can show, from one node to the next, or from outside
  // the graph (the event stream, the browser asking for a fresh reload)
  private enum Move:
    case FromStream, ToConsidered, ToIgnored, ToWaiting, Direct, FromBrowser,
      ToServer, ToAnswered, ToFailed, ToCancelled, ToScreen

  // The nodes of the graph, one per place an event or a load can be in
  private enum Node:
    case Received, Considered, NotConsidered, Waiting, OnServer, Failed,
      Cancelled, Answered, OnScreen

  private enum Action:
    case AddStep(step: TraceStep)
    case Next
    case Reset

  // The controls: Jump to latest (starts the stepper again from now),
  // Play / Pause, the speed, Show ignored, Next step (off while playing)
  private def renderControls(
      stepperS: Signal[RefreshStepper[?]],
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
      stepperS: Signal[RefreshStepper[?]],
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
  private def renderCurrentStep(stepper: RefreshStepper[?]): HtmlElement =
    div(
      cls := "stepper__current",
      span(cls := "stepper__label", "Current step"),
      stepper.currentStep match
        case Shown.Trace(step) =>
          span(
            cls := "stepper__step",
            TraceStep.describe(step)
          )
        case Shown.Display(loadNumber) =>
          span(
            cls := "stepper__step stepper__step--shown",
            s"The figures of reload $loadNumber are on screen"
          )
        case Shown.Idle =>
          span(
            cls := "stepper__step stepper__step--none",
            "Waiting for the next step"
          )
    )

  // One node per place, laid out as in the mockup: the server's lane on top,
  // the browser's below. Not considered, Failed and Cancelled only show when
  // something is in them
  private def renderGraph(stepper: RefreshStepper[?]): HtmlElement =
    val pipeline = stepper.pipeline
    val (now, lit) = highlighted(stepper.currentStep)

    def eventsAt(place: EventPlace): List[Long] =
      pipeline.events.toList.collect { case (position, `place`) =>
        position
      }.sorted

    def loadsWhere(test: LoadPlace[?] => Boolean): List[(Int, Load[?])] =
      pipeline.loads.toList.filter((_, load) => test(load.place)).sortBy(_._1)

    val notConsidered = eventsAt(EventPlace.NotConsidered)
    val failed = loadsWhere(_ == LoadPlace.Failed)
    val cancelled = loadsWhere(_ == LoadPlace.Cancelled)

    def node(which: Node, title: String, body: Seq[HtmlElement]) =
      renderNode(which, title, now.contains(which), body)

    div(
      cls := "stepper__graph-scroll",
      div(
        cls := "stepper__graph",
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
          notConsidered.nonEmpty,
          failed.nonEmpty,
          cancelled.nonEmpty
        ),
        node(
          Node.Received,
          "Arrived",
          eventsAt(EventPlace.Received).map(renderEvent)
        ),
        node(
          Node.Considered,
          "Needs a reload",
          eventsAt(EventPlace.Considered).map(renderEvent)
        ),
        Option.when(notConsidered.nonEmpty)(
          node(
            Node.NotConsidered,
            "Ignored",
            notConsidered.map(renderEvent)
          )
        ),
        node(
          Node.Waiting,
          "Waiting for a reload",
          loadsWhere(_ == LoadPlace.Pending).map((number, load) =>
            renderLoad(number, load)
          )
        ),
        node(
          Node.OnServer,
          "Server computing",
          loadsWhere(_ == LoadPlace.InFlight).map((number, load) =>
            renderLoad(number, load)
          )
        ),
        Option.when(failed.nonEmpty)(
          node(
            Node.Failed,
            "Reload failed",
            failed.map((number, load) => renderLoad(number, load))
          )
        ),
        Option.when(cancelled.nonEmpty)(
          node(
            Node.Cancelled,
            "Reload cancelled",
            cancelled.map((number, load) => renderLoad(number, load))
          )
        ),
        node(
          Node.Answered,
          "Figures received",
          loadsWhere {
            case LoadPlace.Answered(_, _) => true
            case _                        => false
          }.map((number, load) => renderLoad(number, load))
        ),
        node(
          Node.OnScreen,
          "On screen",
          loadsWhere(_ == LoadPlace.Displayed).map((number, load) =>
            renderLoad(number, load)
          )
        )
      )
    )

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

  // The arrows, in the graph's own coordinates (see stepper.css). The usual
  // moves are always drawn, except towards a hidden node; the others (an
  // event coming from the server, a reload asked by the browser, a reload
  // launched at once) only during the step that makes them. The moves of the
  // current step are amber, drawn last so that they sit on top
  private def renderArrows(
      lit: Set[Move],
      notConsidered: Boolean,
      failed: Boolean,
      cancelled: Boolean
  ): SvgElement =
    val usual = List(
      Some(Move.ToConsidered),
      Option.when(notConsidered)(Move.ToIgnored),
      Some(Move.ToWaiting),
      Some(Move.ToServer),
      Some(Move.ToAnswered),
      Option.when(failed)(Move.ToFailed),
      Option.when(cancelled)(Move.ToCancelled),
      Some(Move.ToScreen)
    ).flatten
    val occasional =
      List(Move.FromStream, Move.Direct, Move.FromBrowser).filter(lit.contains)
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
        svg.markerEnd := (if isNow then "url(#stepper-arrow-head-now)"
                          else "url(#stepper-arrow-head)")
      )

    svg.svg(
      svg.cls := "stepper__arrows",
      svg.width := "1082",
      svg.height := "298",
      svg.viewBox := "0 0 1082 298",
      svg.fill := "none",
      aria.hidden := true,
      svg.defs(
        marker("stepper-arrow-head", "stepper__arrow-head"),
        marker(
          "stepper-arrow-head-now",
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
      case Move.FromStream   => "M147 34 L147 118"
      case Move.ToConsidered => "M222 162 L242 162"
      case Move.ToIgnored    => "M147 204 L147 256 L242 256"
      case Move.ToWaiting    => "M394 162 L414 162"
      case Move.Direct       => "M319 120 L319 40 L586 40"
      case Move.FromBrowser  => "M663 180 L663 96"
      case Move.ToServer     => "M566 150 C 578 150 576 70 586 70"
      case Move.ToAnswered   => "M738 70 C 750 70 748 150 758 150"
      case Move.ToFailed     => "M738 30 L758 30"
      case Move.ToCancelled  => "M663 10 L663 3 L1007 3 L1007 8"
      case Move.ToScreen     => "M910 162 L930 162"

  // Where an occasional arrow comes from, written at its start
  private def moveLabel(move: Move): Option[(String, Int, Int)] =
    move match
      case Move.FromStream  => Some(("event stream", 147, 28))
      case Move.FromBrowser => Some(("opened or changed", 663, 196))
      case _                => None

  // An event not in a load: its dot and its position
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

  // A load: a capsule holding the dots of the events it serves
  private def renderLoad(number: Int, load: Load[?]): HtmlElement =
    val modifier = load.place match
      case LoadPlace.Pending        => "pending"
      case LoadPlace.InFlight       => "in-flight"
      case LoadPlace.Answered(_, _) => "answered"
      case LoadPlace.Displayed      => "displayed"
      case LoadPlace.Failed         => "failed"
      case LoadPlace.Cancelled      => "cancelled"
    span(
      cls := s"stepper__load stepper__load--$modifier",
      span(cls := "stepper__load-name", s"Reload $number"),
      if load.events.isEmpty then
        List(span(cls := "stepper__load-note", "no event"))
      else load.events.toList.sorted.map(renderDot)
    )

  // What the current step changed, drawn in amber: the nodes it put
  // something in, and the moves it made
  private def highlighted(shown: Shown): (Set[Node], Set[Move]) =
    shown match
      case Shown.Idle        => (Set.empty, Set.empty)
      case Shown.Display(_)  => (Set(Node.OnScreen), Set(Move.ToScreen))
      case Shown.Trace(step) =>
        step.kind match
          case TraceStep.Kind.Received =>
            (Set(Node.Received), Set(Move.FromStream))
          case TraceStep.Kind.Considered(_) =>
            (Set(Node.Considered), Set(Move.ToConsidered))
          case TraceStep.Kind.NotConsidered(_) =>
            (Set(Node.NotConsidered), Set(Move.ToIgnored))
          case TraceStep.Kind.Reload(_, outcome) =>
            outcome match
              case Outcome.InitialLoad(_, _) =>
                (
                  Set(Node.OnServer, Node.Cancelled),
                  Set(Move.FromBrowser, Move.ToCancelled)
                )
              case Outcome.Load(_, _, _) =>
                (Set(Node.OnServer), Set(Move.Direct))
              case Outcome.Batched(_, _) =>
                (Set(Node.Waiting), Set(Move.ToWaiting))
              case Outcome.Answer(_, _, _) =>
                (Set(Node.Answered), Set(Move.ToAnswered))
              case Outcome.AnswerAndLaunchPending(_, _) =>
                (
                  Set(Node.Answered, Node.OnServer),
                  Set(Move.ToAnswered, Move.ToServer)
                )
              case Outcome.Error(_) =>
                (Set(Node.Failed), Set(Move.ToFailed))
              case Outcome.ErrorAndLaunchPending(_, _) =>
                (
                  Set(Node.Failed, Node.OnServer),
                  Set(Move.ToFailed, Move.ToServer)
                )
          case _ => (Set.empty, Set.empty)

  private def nodeClass(which: Node): String =
    which match
      case Node.Received      => "received"
      case Node.Considered    => "considered"
      case Node.NotConsidered => "not-considered"
      case Node.Waiting       => "waiting"
      case Node.OnServer      => "server"
      case Node.Failed        => "failed"
      case Node.Cancelled     => "cancelled"
      case Node.Answered      => "answered"
      case Node.OnScreen      => "on-screen"

  private def stepsLeft(count: Int): String =
    count match
      case 0 => "no step left"
      case 1 => "1 step left"
      case n => s"$n steps left"
