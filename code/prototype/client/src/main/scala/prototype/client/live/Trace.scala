package prototype.client.live

import prototype.api.EventMessage

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Records the steps an event goes through in the browser, from the stream to
  * the screen, so that they can be shown one by one. One tracer per `LiveFeed`:
  * every live data of a page reaches it through the feed.
  *
  * For now a recorded step is only logged to the console.
  */
final class Tracer:

  // The event being processed, empty between two events. A plain variable:
  // nothing observes it, and the browser handles one event at a time
  private var currentEvent: Option[EventMessage] = None

  /** Whether the steps are recorded. Off at start: nothing is logged, and the
    * clock is not read.
    */
  val enabled: Var[Boolean] = Var(false)

  /** Runs `block` as the processing of `event`: the steps added while it runs
    * belong to this event. Records "Received" before the block and "Shown" once
    * it has returned, or "Failed" if it threw.
    */
  def span[A](event: EventMessage)(block: => A): A =
    currentEvent = Some(event)
    try
      addStep("Received")
      val result = block
      addStep("Shown")
      result
    catch
      case error: Throwable =>
        addStep("Failed")
        throw error
    finally currentEvent = None

  /** Records that the event being processed reached `step`, at the time of the
    * call (milliseconds since the page loaded). A step added while no event is
    * being processed is recorded as such.
    */
  def addStep(step: String): Unit =
    if enabled.now() then
      val at = dom.window.performance.now()
      val event = currentEvent match
        case Some(event) => s"event ${event.position} (${event.eventType})"
        case None        => "outside any event"
      dom.console.log(f"[trace] $step: $event at $at%.1f ms")

  /** Runs `block` and records `step` with the time the block took. */
  def timed[A](step: String)(block: => A): A =
    if enabled.now() then
      val start = dom.window.performance.now()
      val result = block
      val duration = dom.window.performance.now() - start
      addStep(f"$step in $duration%.1f ms")
      result
    else block
