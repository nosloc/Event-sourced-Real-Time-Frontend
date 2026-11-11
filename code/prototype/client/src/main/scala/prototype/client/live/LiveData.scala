package prototype.client.live

import com.raquo.laminar.api.L.*

/** A value kept up to date from the server, as a view reads it. How it is kept
  * up to date (events replayed onto a snapshot, or a reload on the events that
  * matter) is left to the implementation, so a view can switch from one to the
  * other without changing.
  *
  * Live values compose like signals (`map`, `combineWith`,
  * `withCurrentValueOf`): a value computed from others is present once they all
  * are, loading while any of them loads, and shows the first of their errors.
  *
  * A is the type of the value.
  */
trait LiveData[A]:

  self =>

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
    * value nothing reads stops, and the events the feed sends meanwhile are not
    * kept.
    */
  def keepLoaded: Mod[HtmlElement] =
    valueS --> Observer.empty

  // The members of the values built below are vals, not defs: each reader then
  // shares one signal, so `f` runs once per change, not once per reader

  /** The same live value, transformed. */
  final def map[B](f: A => B): LiveData[B] =
    new LiveData[B]:
      val valueS: Signal[Option[B]] = self.valueS.map(_.map(f))
      val loadingS: Signal[Boolean] = self.loadingS
      val errorS: Signal[Option[Throwable]] = self.errorS

  /** Two live values combined by `f`, recomputed when either changes: present
    * once both are, loading while either loads, this one's error first.
    */
  final def combineWithFn[B, C](that: LiveData[B])(
      f: (A, B) => C
  ): LiveData[C] =
    new LiveData[C]:
      val valueS: Signal[Option[C]] =
        self.valueS.combineWithFn(that.valueS)(_.zip(_).map(f.tupled))
      val loadingS: Signal[Boolean] =
        self.loadingS.combineWithFn(that.loadingS)(_ || _)
      val errorS: Signal[Option[Throwable]] =
        self.errorS.combineWithFn(that.errorS)(_.orElse(_))

  final def combineWith[B](that: LiveData[B]): LiveData[(A, B)] =
    combineWithFn(that)((_, _))

  /** This live value and a signal combined by `f`, recomputed when either
    * changes (a key, a period). The signal is always present, so loading and
    * errors are this one's.
    */
  final def combineWithFn[B, C](that: Signal[B])(
      f: (A, B) => C
  ): LiveData[C] =
    new LiveData[C]:
      val valueS: Signal[Option[C]] =
        self.valueS.combineWithFn(that)((a, b) => a.map(f(_, b)))
      val loadingS: Signal[Boolean] = self.loadingS
      val errorS: Signal[Option[Throwable]] = self.errorS

  final def combineWith[B](that: Signal[B]): LiveData[(A, B)] =
    combineWithFn(that)((_, _))

  /** This live value with the current value of a signal, recomputed only when
    * this one changes: a change of the signal alone does not recompute it.
    */
  final def withCurrentValueOf[B](that: Signal[B]): LiveData[(A, B)] =
    new LiveData[(A, B)]:
      val valueS: Signal[Option[(A, B)]] =
        self.valueS.withCurrentValueOf(that).map((a, b) => a.map((_, b)))
      val loadingS: Signal[Boolean] = self.loadingS
      val errorS: Signal[Option[Throwable]] = self.errorS

end LiveData

object LiveData:

  /** A signal as a live value: always present, never loading, never failing. */
  def fromSignal[A](signal: Signal[A]): LiveData[A] =
    new LiveData[A]:
      val valueS: Signal[Option[A]] = signal.map(Some(_))
      val loadingS: Signal[Boolean] = Val(false)
      val errorS: Signal[Option[Throwable]] = Val(None)

  /** The live value of the source `f` picks for the current key. Only that
    * source runs: a key change drops the previous one, and the new one starts
    * from scratch (it loads, unless something else keeps it running).
    */
  def switching[K, A](
      keyS: Signal[K]
  )(f: K => LiveData[A]): LiveData[A] =
    new LiveData[A]:
      // One source per key change, shared by the three members
      private val current: Signal[LiveData[A]] = keyS.distinct.map(f)
      val valueS: Signal[Option[A]] = current.flatMapSwitch(_.valueS)
      val loadingS: Signal[Boolean] = current.flatMapSwitch(_.loadingS)
      val errorS: Signal[Option[Throwable]] = current.flatMapSwitch(_.errorS)
