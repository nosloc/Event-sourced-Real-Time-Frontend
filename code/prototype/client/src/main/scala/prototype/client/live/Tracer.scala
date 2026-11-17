package prototype.client.live

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Records the steps an event goes through in the browser, from the stream to
  * the screen, so that the step-by-step panel can walk through them. One tracer
  * per `LiveFeed`: every live data of the page reaches it through the feed. It
  * keeps no state: each step says which event it belongs to. It records only
  * while `enabled` is on, the step-by-step mode.
  */
final class Tracer:

  // Whether the tracer is on or off
  val enabled: Var[Boolean] = Var(false)

  // The bus that records the steps
  private val bus: EventBus[TraceStep] = new EventBus

  // The exposed stream of steps, for the step-by-step panel
  val steps: EventStream[TraceStep] = bus.events

  // Easy way to log the steps to the console, for debugging
  def logToConsole: Mod[HtmlElement] =
    steps --> (step => dom.console.log(s"[trace] ${TraceStep.describe(step)}"))

  /** Records a step in the trace only if the tracer is enabled. Otherwise it
    * does nothing
    *
    * @param kind
    *   the kind of step
    * @param position
    *   the position of the event the step belongs to
    */
  def record(kind: TraceStep.Kind, position: Long): Unit =
    record(kind, Some(position))

  /** Records a step in the trace only if the tracer is enabled. Otherwise it
    * does nothing
    *
    * @param kind
    *   the kind of step
    * @param position
    *   the position of the event the step belongs to, or None when no event is
    *   behind it
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

  /** Run a piece of code and record a timed step
    *
    * @param kind
    *   builds the kind of step from the result of the code
    * @param position
    *   the position of the event the step belongs to, or None when no event is
    *   behind it
    * @param block
    *   the code to run
    * @return
    *   the result of the code after running it and recording the step
    */
  def timed[A](kind: A => TraceStep.Kind, position: Option[Long])(
      block: => A
  ): A =
    if enabled.now() then
      val start = dom.window.performance.now()
      val result = block
      val end = dom.window.performance.now()
      bus.emit(
        TraceStep(
          kind = kind(result),
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
    // The feed received the event, before anything reads it: its type and
    // its payload as sent (JSON)
    case Received(eventType: String, payload: String)
    // Everything the event triggered synchronously is done. On the Browser
    // level the screen is written; on the Server level a request may have left
    case Ended
    // Browser level: the replayed live data `source` decoded the event
    case Decoded(source: String)
    // Browser level: the event was applied to the value of `source`, or only
    // kept aside (still loading, or already in the snapshot)
    case Folded(source: String, applied: Boolean)
    // Browser level: `source` asked the server for its snapshot (first load,
    // key change, reconnect); `request` is what was asked
    case SnapshotRequested(source: String, request: String)
    // Browser level: the snapshot of `source` arrived; it includes the events
    // up to `position`, and the ones kept aside after it are folded on top.
    // `size` is how many items it holds, when it is a list
    case SnapshotLoaded(source: String, position: Long, size: Option[Int])
    // Browser level: the snapshot of `source` could not be loaded
    case SnapshotFailed(source: String)
    // Browser level: the figures of `dashboard` were computed
    case FiguresComputed(dashboard: String, newFigures: Any)
    // Server level: the event makes the live data `source` out of date
    case Considered(source: String)
    // Server level: the event doesn't make the live data `source` out of date
    case NotConsidered(source: String)
    // Server level: what the refresh model of `source` did (a load launched,
    // an event batched, an answer); the outcome holds the load's number
    case Reload(source: String, outcome: RefreshModel.Outcome[Any, Any])

  /** A step as one line of text, for the console and the step-by-step panel.
    */
  def describe(step: TraceStep): String =
    val event = step.position.fold("no event")(position => s"event $position")
    val duration = step.duration.fold("")(d => f" in $d%.1f ms")
    f"${title(step)}$duration: $event at ${step.at}%.1f ms"

  /** What a step did, as a few words, without its event nor its time. */
  def title(step: TraceStep): String =
    import RefreshModel.Outcome
    step.kind match
      case Kind.Received(eventType, _)  => s"Received $eventType"
      case Kind.Ended                   => "Ended"
      case Kind.Decoded(source)         => s"Decoded by $source"
      case Kind.Folded(source, applied) =>
        if applied then s"Folded into $source" else s"Not folded into $source"
      case Kind.SnapshotRequested(source, _) =>
        s"Snapshot of $source requested"
      case Kind.SnapshotLoaded(source, position, _) =>
        s"Snapshot of $source received, up to event $position"
      case Kind.SnapshotFailed(source)        => s"Snapshot of $source failed"
      case Kind.FiguresComputed(dashboard, _) =>
        s"Figures of $dashboard computed"
      case Kind.Considered(source)      => s"Considered for $source"
      case Kind.NotConsidered(source)   => s"Not considered for $source"
      case Kind.Reload(source, outcome) =>
        outcome match
          case Outcome.InitialLoad(key, loadNumber) =>
            s"Launched initial load $loadNumber of $source for $key"
          case Outcome.Load(_, _, loadNumber) =>
            s"Launched load $loadNumber of $source"
          case Outcome.Batched(_, servedBy) =>
            s"Batched, to be served by load $servedBy of $source"
          case Outcome.Answer(_, loadNumber, _) =>
            s"Received server data for load $loadNumber of $source"
          case Outcome.AnswerAndLaunchPending(answer, load) =>
            s"Received server data for load ${answer.loadNumber} of $source, " +
              s"launched pending load ${load.loadNumber}"
          case Outcome.Error(loadNumber) =>
            s"Load $loadNumber of $source failed"
          case Outcome.ErrorAndLaunchPending(error, load) =>
            s"Load ${error.loadNumber} of $source failed, " +
              s"launched pending load ${load.loadNumber}"
