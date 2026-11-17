package prototype.client.views.dashboard

import prototype.client.live.{LiveData, Stepper, TraceStep, Tracer}
import prototype.client.views.components.PopoverPicker
import prototype.client.views.components.PopoverPicker.Item

import com.raquo.laminar.api.L.*

/** The step-by-step mode of one dashboard, the same on both levels: the stepper
  * driven by the steps recorded and the controls, the live data it holds until
  * each display step, and the panel. What differs per level is passed in: the
  * stepper (its rules, through `Stepper`), how it starts, and how its current
  * step and its graph are drawn.
  *
  * S is the stepper's state, A the type of the dashboard's value.
  *
  * @param start
  *   the stepper at the start, and on each Jump to latest, from the value on
  *   screen
  * @param drawn
  *   what the panel draws of a stepper: the current step and the graph are
  *   drawn again only when it changes, not on every step recorded
  */
final class StepByStepView[S <: Stepper[S, A], A](
    tracer: Tracer,
    source: String,
    val live: LiveData[A],
    start: Option[A] => S,
    drawn: S => Any,
    renderCurrentStep: S => HtmlElement,
    renderGraph: S => HtmlElement
) extends StepByStep[A]:
  import StepByStepView.*

  // How fast Play goes
  private val speedVar: Var[Speed] = Var(Speeds.head)
  private val playingVar: Var[Boolean] = Var(false)
  // Off: Next skips the events this dashboard ignores
  private val showIgnoredVar: Var[Boolean] = Var(false)

  private val newSteps: EventStream[Action] =
    tracer.steps.map(Action.AddStep(_))
  private val pressedNextBus: EventBus[Action] = new EventBus
  private val pressedLatestBus: EventBus[Action] = new EventBus
  private val playTicks: EventStream[Action] =
    speedVar.signal
      .combineWith(playingVar.signal)
      .flatMapSwitch((speed, play) =>
        if play then
          EventStream.periodic(speed.every).mapTo(Action.Steps(speed.steps))
        else EventStream.empty
      )

  private val nexts = pressedNextBus.events.mergeWith(playTicks)

  private val stepperS: Signal[S] =
    newSteps
      .mergeWith(nexts, pressedLatestBus.events)
      .mergeWith(EventStream.fromValue(Action.Reset)) // Start with a reset
      .withCurrentValueOf(showIgnoredVar.signal, live.valueS)
      .scanLeft(start(None)) { case (stepper, (action, showIgnored, real)) =>
        action match
          case Action.AddStep(traceStep) => stepper.addStep(traceStep)
          case Action.Next         => stepper.next(skipIgnored = !showIgnored)
          case Action.Steps(count) =>
            steps(stepper, count, skipIgnored = !showIgnored)
          case Action.NextChange =>
            untilChange(stepper, skipIgnored = !showIgnored)
          case Action.Reset => start(real)
      }

  // `count` Nexts at once, drawn once (fewer if no step is left)
  private def steps(from: S, count: Int, skipIgnored: Boolean): S =
    @scala.annotation.tailrec
    def go(stepper: S, left: Int): S =
      if left == 0 || !stepper.hasNextStep(skipIgnored) then stepper
      else go(stepper.next(skipIgnored), left - 1)
    go(from, count)

  // Next until the figures on screen change, or no step is left: one jump
  // over a burst, drawn once. Bounded, so that it never freezes the page
  private def untilChange(from: S, skipIgnored: Boolean): S =
    @scala.annotation.tailrec
    def go(stepper: S, left: Int): S =
      if left == 0 || !stepper.hasNextStep(skipIgnored) then stepper
      else
        val next = stepper.next(skipIgnored)
        if next.displayed != from.displayed then next else go(next, left - 1)
    go(from, MaxJump)

  // The stepper as the panel draws it: a recorded step only grows its
  // history, which the current step and the graph do not show
  private val shownS: Signal[S] = stepperS.distinctBy(drawn)

  override val steppedData: LiveData[A] = new LiveData[A]:
    val valueS: Signal[Option[A]] = stepperS.map(_.displayed).distinct
    val errorS: Signal[Option[Throwable]] = Val(None)
    val loadingS: Signal[Boolean] = Val(false)

  override def panel: HtmlElement =
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
            text <-- stepperS
              .combineWith(showIgnoredVar.signal)
              .map((s, showIgnored) => stepsLeft(s.stepsLeft(!showIgnored)))
          )
        ),
        renderControls(
          stepperS
            .combineWith(showIgnoredVar.signal)
            .map((s, showIgnored) => s.hasNextStep(!showIgnored)),
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
        child <-- shownS.map(renderCurrentStep)
      ),
      child <-- shownS.map(renderGraph)
    )

object StepByStepView:

  private enum Action:
    case AddStep(step: TraceStep)
    case Next
    // Several Nexts at once, while playing fast
    case Steps(count: Int)
    // Next until the figures on screen change
    case NextChange
    case Reset

  // The most steps one "next figures change" takes
  private val MaxJump = 100000

  /** A speed of Play: a tick every `every` milliseconds, `steps` Nexts per
    * tick. Above 20 steps a second the page is drawn once per tick, not once
    * per step.
    */
  final case class Speed(every: Int, steps: Int)

  // From the slowest; the first is the one Play starts with
  private val Speeds: List[Speed] = List(
    Speed(1000, 1),
    Speed(250, 1),
    Speed(50, 1),
    Speed(50, 10),
    Speed(50, 100)
  )

  private def speedName(speed: Speed): String =
    val perSecond = speed.steps * 1000 / speed.every
    // Thousands written the Swiss way, as the amounts: 2'000
    val number =
      if perSecond >= 1000 then
        s"${perSecond / 1000}'${f"${perSecond % 1000}%03d"}"
      else perSecond.toString
    if perSecond == 1 then "1 step/s" else s"$number steps/s"

  // The controls: Jump to latest (starts the stepper again from now),
  // Play / Pause, the speed, Show ignored, Next step and next figures change
  // (both off while playing)
  private def renderControls(
      hasNextS: Signal[Boolean],
      playingVar: Var[Boolean],
      speedVar: Var[Speed],
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
      renderNextButton(hasNextS, playingVar.signal, pressedNext),
      renderNextChangeButton(hasNextS, playingVar.signal, pressedNext)
    )

  private def renderJumpButton(pressedLatest: Observer[Action]): HtmlElement =
    button(
      typ := "button",
      cls := "btn stepper__jump",
      "Jump to latest",
      onClick.mapTo(Action.Reset) --> pressedLatest
    )

  // Play while paused, Pause while playing
  private def renderPlayButton(playingVar: Var[Boolean]): HtmlElement =
    button(
      typ := "button",
      cls := "btn stepper__play",
      cls("stepper__play--on") <-- playingVar.signal,
      aria.label <-- playingVar.signal.map(playing =>
        if playing then "Pause" else "Play"
      ),
      child <-- playingVar.signal.map(playIcon),
      onClick --> (_ => playingVar.update(!_))
    )

  // How fast Play goes, in steps per second
  private def renderSpeedSelector(speedVar: Var[Speed]): HtmlElement =
    PopoverPicker(
      id = "step-speed-selector",
      label = "Speed",
      items = Val(Speeds.map(speed => Item(speed, speedName(speed)))),
      selectedVar = speedVar
    )

  // On: every step, the events this dashboard ignores too
  private def renderShowIgnoredToggle(
      showIgnoredVar: Var[Boolean]
  ): HtmlElement =
    button(
      typ := "button",
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
      hasNextS: Signal[Boolean],
      playingS: Signal[Boolean],
      pressedNext: Observer[Action]
  ): HtmlElement =
    button(
      typ := "button",
      cls := "btn btn--primary stepper__next",
      disabled <-- hasNextS
        .combineWith(playingS)
        .map((hasNext, play) => play || !hasNext),
      "Next step",
      span(aria.hidden := true, "→"),
      onClick.mapTo(Action.Next) --> pressedNext
    )

  // Secondary to Next step, like Play: an icon, its name in the tooltip
  private def renderNextChangeButton(
      hasNextS: Signal[Boolean],
      playingS: Signal[Boolean],
      pressedNext: Observer[Action]
  ): HtmlElement =
    button(
      typ := "button",
      cls := "btn stepper__play stepper__skip",
      title := "Next figures change: step until the figures on screen change",
      aria.label := "Next figures change",
      disabled <-- hasNextS
        .combineWith(playingS)
        .map((hasNext, play) => play || !hasNext),
      skipIcon,
      onClick.mapTo(Action.NextChange) --> pressedNext
    )

  // A triangle and a bar: skip forward
  private def skipIcon: SvgElement =
    svg.svg(
      svg.width := "14",
      svg.height := "14",
      svg.viewBox := "0 0 14 14",
      svg.fill := "none",
      aria.hidden := true,
      svg.path(svg.d := "M2.5 2.5v9l7-4.5z M11.5 2.5v9")
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

  private def stepsLeft(count: Int): String =
    count match
      case 0 => "no step left"
      case 1 => "1 step left"
      case n => s"$n steps left"
