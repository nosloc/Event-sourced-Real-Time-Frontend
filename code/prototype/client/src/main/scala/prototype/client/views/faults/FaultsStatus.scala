package prototype.client.views.faults

import prototype.api.Faults

import com.raquo.laminar.api.L.*

/** Tells the rest of the page when the faults change: the Faults page emits
  * the server's answer to each change (Apply, Turn all off), and the "Faults
  * on" pill in the top bar follows it. A channel, not a state: it holds
  * nothing, the server stays the only place that knows the faults.
  *
  * Same tab only: another window learns the change when it reads the faults
  * again (on its next page).
  */
object FaultsStatus:

  private val appliedBus: EventBus[Faults] = new EventBus

  /** The faults the server holds, after each change. */
  val applied: EventStream[Faults] = appliedBus.events

  def emit(faults: Faults): Unit = appliedBus.emit(faults)
