package prototype.client.live

import com.raquo.laminar.api.L.*

/** A value kept up to date from the server, as a view reads it. How it is kept
  * up to date (events replayed onto a snapshot, or a reload on the events that
  * matter) is left to the implementation, so a view can switch from one to the
  * other without changing.
  *
  * A is the type of the value.
  */
trait LiveData[A]:

  /** The latest value, or `None` until the first load succeeds. While a reload
    * runs, the previous value stays.
    */
  def valueS: Signal[Option[A]]

  /** Whether a load is running. */
  def loadingS: Signal[Boolean]

  /** The error of the last load, if it failed; cleared when a load starts. The
    * previous value stays, so the error can be shown next to it.
    */
  def errorS: Signal[Option[Throwable]]

  /** Mount this on an element that stays on screen. The signals are lazy, so a
    * value nothing reads stops: the feed does not keep the events it sends
    * meanwhile, and a value first read after the stream opened never loads.
    */
  def keepLoaded: Mod[HtmlElement] =
    valueS --> Observer.empty
