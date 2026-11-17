package prototype.client.views.dashboard

import prototype.client.live.{LiveData, RefreshStepper, TraceStep, Tracer}
import prototype.client.live.RefreshModel.Outcome
import prototype.client.live.RefreshStepper.{EventPlace, Load, LoadPlace, Shown}
import prototype.client.views.components.{StepCard, StepGraph}

import com.raquo.laminar.api.L.*

object ServerStepByStep:

  /** The step-by-step mode of a dashboard on the server level: its live data
    * reloaded from the server, held by a `RefreshStepper`, drawn on the
    * server's graph.
    */
  def apply[A](
      tracer: Tracer,
      source: String,
      live: LiveData[A]
  ): StepByStep[A] =
    StepByStepView[RefreshStepper[A], A](
      tracer,
      source,
      live,
      start = real => RefreshStepper.empty[A](source).copy(displayed = real),
      // A recorded step only grows `history`, which the panel does not draw
      drawn = stepper => (stepper.currentStep, stepper.pipeline),
      renderCurrentStep = renderCurrentStep,
      renderGraph = renderGraph
    )

  // The moves an arrow can show, from one node to the next, or from outside
  // the graph (the event stream, the browser asking for a fresh reload)
  private enum Move:
    case FromStream, ToConsidered, ToIgnored, ToWaiting, Direct, FromBrowser,
      ToServer, ToAnswered, ToFailed, ToDropped, ToScreen

  // The nodes of the graph, one per place an event or a load can be in
  private enum Node:
    case Received, Considered, NotConsidered, Waiting, OnServer, Failed,
      Dropped, Answered, OnScreen

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

  // One node per place, laid out in two lanes: the server's on top, the
  // browser's below. Not considered, Failed and Dropped only show when
  // something is in them
  private def renderGraph(stepper: RefreshStepper[?]): HtmlElement =
    val pipeline = stepper.pipeline
    val (now, lit) = highlighted(stepper.currentStep)

    // What was clicked, whose card is open: drawn again on each Next, so a
    // card closes when the panel steps
    val openedVar = Var(Option.empty[Opened])

    def event(position: Long): HtmlElement =
      StepCard.clickable(
        StepCard.eventAnchor(position),
        s"Details of event $position",
        () => openedVar.set(Some(Opened.Event(position)))
      )(StepGraph.event(position))

    def clickableLoad(number: Int, load: Load[?]): HtmlElement =
      StepCard.clickable(
        s"step-load-$number",
        s"Details of reload $number",
        () => openedVar.set(Some(Opened.Load(number)))
      )(renderLoad(number, load))

    def eventsAt(place: EventPlace): List[Long] =
      pipeline.events.toList.collect { case (position, `place`) =>
        position
      }.sorted

    def loadsWhere(test: LoadPlace[?] => Boolean): List[(Int, Load[?])] =
      pipeline.loads.toList.filter((_, load) => test(load.place)).sortBy(_._1)

    val notConsidered = eventsAt(EventPlace.NotConsidered)
    val failed = loadsWhere(_ == LoadPlace.Failed)
    val dropped = loadsWhere(_ == LoadPlace.Dropped)

    def node(which: Node, title: String, body: Seq[HtmlElement]) =
      StepGraph.node(nodeClass(which), title, now.contains(which), body)

    div(
      cls := "stepper__graph-scroll",
      div(
        cls := "stepper__graph",
        StepGraph.lanes,
        renderArrows(
          lit,
          notConsidered.nonEmpty,
          failed.nonEmpty,
          dropped.nonEmpty
        ),
        node(
          Node.Received,
          "Arrived",
          eventsAt(EventPlace.Received).map(event)
        ),
        node(
          Node.Considered,
          "Needs a reload",
          eventsAt(EventPlace.Considered).map(event)
        ),
        Option.when(notConsidered.nonEmpty)(
          node(
            Node.NotConsidered,
            "Ignored",
            notConsidered.map(event)
          )
        ),
        node(
          Node.Waiting,
          "Waiting for a reload",
          loadsWhere(_ == LoadPlace.Pending).map((number, load) =>
            clickableLoad(number, load)
          )
        ),
        node(
          Node.OnServer,
          "Server computing",
          loadsWhere(_ == LoadPlace.InFlight).map((number, load) =>
            clickableLoad(number, load)
          )
        ),
        Option.when(failed.nonEmpty)(
          node(
            Node.Failed,
            "Reload failed",
            failed.map((number, load) => clickableLoad(number, load))
          )
        ),
        Option.when(dropped.nonEmpty)(
          node(
            Node.Dropped,
            // Not a real cancellation: the request still runs on the
            // server, the browser only ignores its answer
            "Answer dropped",
            dropped.map((number, load) => clickableLoad(number, load))
          )
        ),
        node(
          Node.Answered,
          "Figures received",
          loadsWhere {
            case LoadPlace.Answered(_) => true
            case _                     => false
          }.map((number, load) => clickableLoad(number, load))
        ),
        node(
          Node.OnScreen,
          "On screen",
          loadsWhere(_ == LoadPlace.Displayed).map((number, load) =>
            clickableLoad(number, load)
          )
        ),
        StepCard.popoverCard(
          openedVar.signal.map(_.map(anchorOf)),
          openedVar.signal.map(_.map(card(stepper, _)))
        )
      )
    )

  // What a click opened a card on
  private enum Opened:
    case Event(position: Long)
    case Load(number: Int)

  private def anchorOf(opened: Opened): String =
    opened match
      case Opened.Event(position) => StepCard.eventAnchor(position)
      case Opened.Load(number)    => s"step-load-$number"

  // The card of what was clicked, from what the stepper kept about it
  private def card(stepper: RefreshStepper[?], opened: Opened): HtmlElement =
    opened match
      case Opened.Event(position) =>
        StepCard.event(position, stepper.eventCards.get(position))
      case Opened.Load(number) =>
        val events = stepper.pipeline.loads
          .get(number)
          .fold("none")(load =>
            if load.events.isEmpty then "none"
            else load.events.toList.sorted.mkString(", ")
          )
        val pending = stepper.pipeline.loads
          .get(number)
          .exists(_.place == LoadPlace.Pending)
        stepper.loadCards.get(number) match
          // Not launched yet: it waits for the reload in flight
          case None if pending =>
            StepCard.frame(
              s"Reload $number",
              StepCard.facts(
                "State" -> "waiting: it leaves once the reload in flight answers",
                "Events it will serve" -> events
              )
            )
          // Launched before the panel opened: nothing recorded about it
          case None =>
            StepCard.frame(
              s"Reload $number",
              StepCard.facts(
                "State" -> "launched before the panel opened",
                "Events it serves" -> events
              )
            )
          case Some(load) =>
            StepCard.frame(
              s"Reload $number",
              StepCard.facts(
                "Request" -> load.request,
                "Events it serves" -> events,
                (if load.failed then "Failed after" else "Round trip") ->
                  StepCard.between(
                    load.launchedAt,
                    load.answeredAt,
                    "in flight"
                  )
              )
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
      dropped: Boolean
  ): SvgElement =
    val usual = List(
      Some(Move.ToConsidered),
      Option.when(notConsidered)(Move.ToIgnored),
      Some(Move.ToWaiting),
      Some(Move.ToServer),
      Some(Move.ToAnswered),
      Option.when(failed)(Move.ToFailed),
      Option.when(dropped)(Move.ToDropped),
      Some(Move.ToScreen)
    ).flatten
    StepGraph.arrows(
      width = 1082,
      height = 298,
      idPrefix = "stepper",
      usual = usual,
      occasional = List(Move.FromStream, Move.Direct, Move.FromBrowser),
      lit = lit,
      path = movePath,
      label = moveLabel
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
      case Move.ToDropped    => "M663 10 L663 3 L1007 3 L1007 8"
      case Move.ToScreen     => "M910 162 L930 162"

  // Where an occasional arrow comes from, written at its start
  private def moveLabel(move: Move): Option[(String, Int, Int)] =
    move match
      case Move.FromStream  => Some(("event stream", 147, 28))
      case Move.FromBrowser => Some(("opened or changed", 663, 196))
      case _                => None

  // A load: a capsule holding the dots of the events it serves
  private def renderLoad(number: Int, load: Load[?]): HtmlElement =
    val modifier = load.place match
      case LoadPlace.Pending     => "pending"
      case LoadPlace.InFlight    => "in-flight"
      case LoadPlace.Answered(_) => "answered"
      case LoadPlace.Displayed   => "displayed"
      case LoadPlace.Failed      => "failed"
      case LoadPlace.Dropped     => "dropped"
    StepGraph.capsule(
      modifier,
      s"Reload $number",
      load.events.toList,
      Option.when(load.events.isEmpty)("no event")
    )

  // What the current step changed, drawn in amber: the nodes it put
  // something in, and the moves it made
  private def highlighted(shown: Shown): (Set[Node], Set[Move]) =
    shown match
      case Shown.Idle        => (Set.empty, Set.empty)
      case Shown.Display(_)  => (Set(Node.OnScreen), Set(Move.ToScreen))
      case Shown.Trace(step) =>
        step.kind match
          case TraceStep.Kind.Received(_, _) =>
            (Set(Node.Received), Set(Move.FromStream))
          case TraceStep.Kind.Considered(_) =>
            (Set(Node.Considered), Set(Move.ToConsidered))
          case TraceStep.Kind.NotConsidered(_) =>
            (Set(Node.NotConsidered), Set(Move.ToIgnored))
          case TraceStep.Kind.Reload(_, outcome) =>
            outcome match
              case Outcome.InitialLoad(_, _) =>
                (
                  Set(Node.OnServer, Node.Dropped),
                  Set(Move.FromBrowser, Move.ToDropped)
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
      case Node.Dropped       => "dropped"
      case Node.Answered      => "answered"
      case Node.OnScreen      => "on-screen"
