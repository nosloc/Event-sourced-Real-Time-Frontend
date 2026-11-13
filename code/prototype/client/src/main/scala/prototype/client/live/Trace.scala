package prototype.client.live

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Records the steps an event goes through in the browser, from the stream to
  * the screen, so that they can be shown one by one. One tracer per `LiveFeed`:
  * every live data of a page reaches it through the feed. The tracer keeps no
  * state: each caller says which event its step belongs to.
  *
  * For now a recorded step is only logged to the console.
  */
final class Tracer:

  /** Whether the steps are recorded. Off at start: nothing is logged, and the
    * clock is not read.
    */
  val enabled: Var[Boolean] = Var(false)

  // Only `record` and `timed` emit; the others read `steps`
  private val bus: EventBus[TraceStep] = new EventBus

  /** The steps recorded, in the order recorded. Nothing while `enabled` is off.
    */
  val steps: EventStream[TraceStep] = bus.events

  /** Prints each recorded step in the console, while the element it is given to
    * is mounted.
    */
  def logToConsole: Mod[HtmlElement] =
    steps --> (step => dom.console.log(s"[trace] ${TraceStep.describe(step)}"))

  /** Records that the event at `position` reached the step `kind`: the tracer
    * reads the clock itself.
    */
  def record(kind: TraceStep.Kind, position: Long): Unit =
    record(kind, Some(position))

  /** The same for a position that may be missing: None for a step with no event
    * behind it (a first load, a company change, the answer of a load).
    */
  def record(kind: TraceStep.Kind, position: Option[Long] = None): Unit =
    if enabled.now() then
      bus.emit(
        TraceStep(
          kind = kind,
          position = position,
          at = dom.window.performance.now(),
          duration = None
        )
      )

  /** Runs `block` and records the step `kind` for the event at `position`, with
    * the time the block took. The step's time is the end of the block.
    */
  def timed[A](kind: TraceStep.Kind, position: Option[Long])(block: => A): A =
    if enabled.now() then
      val start = dom.window.performance.now()
      val result = block
      val end = dom.window.performance.now()
      bus.emit(
        TraceStep(
          kind = kind,
          position = position,
          at = end,
          duration = Some(end - start)
        )
      )
      result
    else block

/** One step recorded by the tracer, as data.
  *
  * @param kind
  *   what happened
  * @param position
  *   the position of the event the step belongs to, or None when no event is
  *   behind it (a first load, a company change, the answer of a load)
  * @param at
  *   when the step was recorded (milliseconds since the page loaded)
  * @param duration
  *   how long the step took in milliseconds, for the steps that are timed
  */
final case class TraceStep(
    kind: TraceStep.Kind,
    position: Option[Long],
    at: Double,
    duration: Option[Double]
)

object TraceStep:

  enum Kind:
    // The feed received the event, before anything reads it
    case Received
    // Everything the event triggered synchronously is done. On the Browser
    // level the screen is written; on the Server level a request may have left
    case Ended
    // Browser level: a replayed live data decoded the event
    case Decoded
    // Browser level: the event was applied to the value, or only kept aside
    // (still loading, or already in the snapshot)
    case Folded(applied: Boolean)
    // Browser level: the figures of `dashboard` were computed
    case FiguresComputed(dashboard: String)
    // Server level: the event makes the live data `source` out of date
    case Considered(source: String)
    // Server level: the event doesn't make the live data `source` out of date
    case NotConsidered(source: String)
    // Server level: what the refresh model of `source` did (a load launched,
    // an event batched, an answer); the outcome holds the load's number
    case Reload(source: String, outcome: RefreshModel.Outcome[Any])

  /** A step as one line of text, for the console and the step-by-step panel.
    */
  def describe(step: TraceStep): String =
    import RefreshModel.Outcome
    val what = step.kind match
      case Kind.Received        => "Received"
      case Kind.Ended           => "Ended"
      case Kind.Decoded         => "Decoded"
      case Kind.Folded(applied) =>
        if applied then "Folded" else "Not folded"
      case Kind.FiguresComputed(dashboard) => s"Figures of $dashboard computed"
      case Kind.Considered(source)         => s"Considered for $source"
      case Kind.NotConsidered(source)      => s"Not considered for $source"
      case Kind.Reload(source, outcome)    =>
        outcome match
          case Outcome.InitialLoad(key, loadNumber) =>
            s"Launched initial load $loadNumber of $source for $key"
          case Outcome.Load(_, _, loadNumber) =>
            s"Launched load $loadNumber of $source"
          case Outcome.Batched(_, servedBy) =>
            s"Batched, to be served by load $servedBy of $source"
          case Outcome.Answer(_, loadNumber) =>
            s"Received server data for load $loadNumber of $source"
          case Outcome.AnswerAndLaunchPending(answer, load) =>
            s"Received server data for load ${answer.loadNumber} of $source, " +
              s"launched pending load ${load.loadNumber}"
          case Outcome.Error(loadNumber) =>
            s"Load $loadNumber of $source failed"
          case Outcome.ErrorAndLaunchPending(error, load) =>
            s"Load ${error.loadNumber} of $source failed, " +
              s"launched pending load ${load.loadNumber}"
    val event = step.position.fold("no event")(position => s"event $position")
    val duration = step.duration.fold("")(d => f" in $d%.1f ms")
    f"$what$duration: $event at ${step.at}%.1f ms"
