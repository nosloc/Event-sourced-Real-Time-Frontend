package prototype.client.views.dashboard

import prototype.client.live.RefreshModel.Outcome
import prototype.client.live.{
  EventPlace,
  Load,
  LoadPlace,
  RefreshStepper,
  TraceStep,
  Tracer
}

import com.raquo.laminar.api.L.*

object ServerStepByStep:
  def apply(tracer: Tracer, source: String): HtmlElement =
    enum Action:
      case AddStep(step: TraceStep)
      case Next

    val newSteps: EventStream[Action] = tracer.steps.map(Action.AddStep(_))
    val nextBus: EventBus[Action] = new EventBus

    val stepper: Signal[RefreshStepper] =
      newSteps
        .mergeWith(nextBus.events)
        .scanLeft(RefreshStepper.empty(source))((stepper, action) =>
          action match
            case Action.AddStep(traceStep) => stepper.addStep(traceStep)
            case Action.Next               => stepper.step()
        )

    div(
      cls := "stepper",
      role := "region",
      aria.label := "Step by step",
      div(
        cls := "stepper__bar",
        div(
          cls := "stepper__heading",
          span(cls := "stepper__title", "Step by step"),
          span(cls := "stepper__source", source),
          span(
            cls := "stepper__left",
            text <-- stepper.map(s => stepsLeft(s.history.size))
          )
        ),
        button(
          tpe := "button",
          cls := "btn btn--primary stepper__next",
          disabled <-- stepper.map(!_.hasNextStep()),
          "Next step",
          span(aria.hidden := true, "→"),
          onClick.mapTo(Action.Next) --> nextBus.writer
        )
      ),
      child <-- stepper.map(renderCurrentStep),
      child <-- stepper.map(renderGraph)
    )

  // The nodes of the graph, one per place an event or a load can be in
  private enum Node:
    case Received, Considered, NotConsidered, Waiting, OnServer, Failed,
      Cancelled, Answered, OnScreen

  // The step just shown, as one line
  private def renderCurrentStep(stepper: RefreshStepper): HtmlElement =
    div(
      cls := "stepper__current",
      span(cls := "stepper__label", "Current step"),
      stepper.currentStep match
        case Some(step) =>
          span(
            cls := "stepper__step",
            TraceStep.describe(step).stripPrefix("[trace] ")
          )
        case None if isDisplayStep(stepper) =>
          span(
            cls := "stepper__step stepper__step--shown",
            "The answer is on screen"
          )
        case None =>
          span(
            cls := "stepper__step stepper__step--none",
            "Waiting for the next step"
          )
    )

  // One node per place, laid out as in the mockup: the server's lane on top,
  // the browser's below. Not considered, Failed and Cancelled only show when
  // something is in them
  private def renderGraph(stepper: RefreshStepper): HtmlElement =
    val pipeline = stepper.pipeline
    val now = changedNodes(stepper)

    def eventsAt(place: EventPlace): List[Long] =
      pipeline.events.toList.collect { case (position, `place`) =>
        position
      }.sorted

    def loadsWhere(test: LoadPlace => Boolean): List[(Int, Load)] =
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
          notConsidered.nonEmpty,
          failed.nonEmpty,
          cancelled.nonEmpty
        ),
        node(
          Node.Received,
          "Received",
          eventsAt(EventPlace.Received).map(renderEvent)
        ),
        node(
          Node.Considered,
          "Considered",
          eventsAt(EventPlace.Considered).map(renderEvent)
        ),
        Option.when(notConsidered.nonEmpty)(
          node(
            Node.NotConsidered,
            "Not considered",
            notConsidered.map(renderEvent)
          )
        ),
        node(
          Node.Waiting,
          "Waiting for a load",
          loadsWhere(_ == LoadPlace.Pending).map((number, load) =>
            renderLoad(number, load)
          )
        ),
        node(
          Node.OnServer,
          "On the server",
          loadsWhere(_ == LoadPlace.InFlight).map((number, load) =>
            renderLoad(number, load)
          )
        ),
        Option.when(failed.nonEmpty)(
          node(
            Node.Failed,
            "Failed",
            failed.map((number, load) => renderLoad(number, load))
          )
        ),
        Option.when(cancelled.nonEmpty)(
          node(
            Node.Cancelled,
            "Cancelled",
            cancelled.map((number, load) => renderLoad(number, load))
          )
        ),
        node(
          Node.Answered,
          "Answer received",
          loadsWhere {
            case LoadPlace.Answered(_) => true
            case _                     => false
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

  // The arrows of the mockup, in the graph's own coordinates (see
  // stepper.css). The ones towards a hidden node are left out
  private def renderArrows(
      notConsidered: Boolean,
      failed: Boolean,
      cancelled: Boolean
  ): SvgElement =
    def arrow(path: String) =
      svg.path(svg.d := path, svg.markerEnd := "url(#stepper-arrow-head)")
    svg.svg(
      svg.cls := "stepper__arrows",
      svg.width := "1082",
      svg.height := "298",
      svg.viewBox := "0 0 1082 298",
      svg.fill := "none",
      aria.hidden := true,
      svg.defs(
        svg.marker(
          svg.idAttr := "stepper-arrow-head",
          svg.viewBox := "0 0 8 8",
          svg.refX := "7",
          svg.refY := "4",
          svg.markerWidth := "8",
          svg.markerHeight := "8",
          svg.orient := "auto",
          svg.path(svg.d := "M0 0 8 4 0 8z", svg.cls := "stepper__arrow-head")
        )
      ),
      svg.g(
        svg.cls := "stepper__arrow",
        // received → considered, → not considered
        arrow("M222 162 L242 162"),
        Option.when(notConsidered)(arrow("M147 204 L147 256 L242 256")),
        // considered → waiting, → straight into a load
        arrow("M394 162 L414 162"),
        arrow("M319 120 L319 40 L586 40"),
        // waiting → on the server
        arrow("M566 150 C 578 150 576 70 586 70"),
        // on the server → answer, → failed, → cancelled
        arrow("M738 70 C 750 70 748 150 758 150"),
        Option.when(failed)(arrow("M738 30 L758 30")),
        Option.when(cancelled)(arrow("M663 10 L663 3 L1007 3 L1007 8")),
        // answer → on screen
        arrow("M910 162 L930 162")
      )
    )

  // An event not in a load: its dot and its position
  private def renderEvent(position: Long): HtmlElement =
    span(
      cls := "stepper__event",
      renderDot(position),
      span(cls := "stepper__position", position.toString)
    )

  // One colour per event, from its position
  private def renderDot(position: Long): HtmlElement =
    span(
      cls := s"stepper__dot stepper__dot--${position % 10}",
      title := s"event $position"
    )

  // A load: a capsule holding the dots of the events it serves
  private def renderLoad(number: Int, load: Load): HtmlElement =
    val modifier = load.place match
      case LoadPlace.Pending     => "pending"
      case LoadPlace.InFlight    => "in-flight"
      case LoadPlace.Answered(_) => "answered"
      case LoadPlace.Displayed   => "displayed"
      case LoadPlace.Failed      => "failed"
      case LoadPlace.Cancelled   => "cancelled"
    span(
      cls := s"stepper__load stepper__load--$modifier",
      span(cls := "stepper__load-name", s"Load $number"),
      if load.events.isEmpty then
        List(span(cls := "stepper__load-note", "no event"))
      else load.events.toList.sorted.map(renderDot)
    )

  // The nodes the current step changed, drawn in amber
  private def changedNodes(stepper: RefreshStepper): Set[Node] =
    stepper.currentStep.map(_.kind) match
      case Some(TraceStep.Kind.Received)           => Set(Node.Received)
      case Some(TraceStep.Kind.Considered(_))      => Set(Node.Considered)
      case Some(TraceStep.Kind.NotConsidered(_))   => Set(Node.NotConsidered)
      case Some(TraceStep.Kind.Reload(_, outcome)) =>
        outcome match
          case Outcome.InitialLoad(_, _) => Set(Node.OnServer, Node.Cancelled)
          case Outcome.Load(_, _, _)     => Set(Node.OnServer)
          case Outcome.Batched(_, _)     => Set(Node.Waiting)
          case Outcome.Answer(_, _)      => Set(Node.Answered)
          case Outcome.AnswerAndLaunchPending(_, _) =>
            Set(Node.Answered, Node.OnServer)
          case Outcome.Error(_)                    => Set(Node.Failed)
          case Outcome.ErrorAndLaunchPending(_, _) =>
            Set(Node.Failed, Node.OnServer)
      case Some(_)                        => Set.empty
      case None if isDisplayStep(stepper) => Set(Node.OnScreen)
      case None                           => Set.empty

  // A step with no trace step behind it that put a load on screen
  private def isDisplayStep(stepper: RefreshStepper): Boolean =
    stepper.currentStep.isEmpty &&
      stepper.pipeline.loads.values.exists(_.place == LoadPlace.Displayed)

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
