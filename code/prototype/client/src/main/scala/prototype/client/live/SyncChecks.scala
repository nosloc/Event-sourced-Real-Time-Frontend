package prototype.client.live

import scala.util.Try

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The application-wide switch of the checks against the server. The checks run
  * only while a `SyncBadge` reads them, and a badge shows only while this is
  * on: off, no check sends a request (for measurements, or a busy store).
  *
  * Off by default; the choice is kept in the browser across reloads.
  */
object SyncChecks:

  private val StorageKey = "pdm.syncChecks"

  // The storage can be missing or refuse access (private window): then the
  // switch starts off and is not kept
  private def stored: Boolean =
    Try(dom.window.localStorage.getItem(StorageKey) == "on").getOrElse(false)

  private val enabledVar: Var[Boolean] = Var(stored)

  val enabledS: Signal[Boolean] = enabledVar.signal

  def set(enabled: Boolean): Unit =
    enabledVar.set(enabled)
    Try(
      dom.window.localStorage
        .setItem(StorageKey, if enabled then "on" else "off")
    )
end SyncChecks
