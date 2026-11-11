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

  /** Records that the event at `position` reached `step`, at the time of the
    * call (milliseconds since the page loaded).
    */
  def record(step: String, position: Long): Unit =
    if enabled.now() then
      val at = dom.window.performance.now()
      dom.console.log(f"[trace] $step: event $position at $at%.1f ms")

  /** Runs `block` and records `step` for the event at `position`, with the time
    * the block took.
    */
  def timed[A](step: String, position: Long)(block: => A): A =
    if enabled.now() then
      val start = dom.window.performance.now()
      val result = block
      val duration = dom.window.performance.now() - start
      record(f"$step in $duration%.1f ms", position)
      result
    else block

  /** Records a `step` with no event behind it (a first load, a company change),
    * at the time of the call.
    */
  def record(step: String): Unit =
    if enabled.now() then
      val at = dom.window.performance.now()
      dom.console.log(f"[trace] $step: no event at $at%.1f ms")
