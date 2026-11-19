package prototype.client.live

import cats.effect.IO

import prototype.client.live.SyncCheckModel.Local

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

  /** Whether the value matches the server, per source it comes from (see
    * `SyncStatus`). The checks run only while this is read.
    */
  def syncS: Signal[SyncStatus]

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
      val syncS: Signal[SyncStatus] = self.syncS

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
      val syncS: Signal[SyncStatus] =
        self.syncS.combineWithFn(that.syncS)(_ ++ _)

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
      val syncS: Signal[SyncStatus] = self.syncS

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
      val syncS: Signal[SyncStatus] = self.syncS

  /** This live value, also compared with the server's own computation of it
    * (`load`), for a value derived in the browser that the server can compute
    * too (a dashboard). Its status gets this check besides those of the sources
    * it comes from.
    *
    * @param name
    *   the name of the check in the status
    * @param keyS
    *   what the server computes it for (a company)
    */
  final def checkedAgainst[K](
      feed: LiveFeed,
      name: String,
      keyS: Signal[K],
      load: K => IO[A],
      same: Same[A] = Same.equal[A]
  ): LiveData[A] =
    new LiveData[A]:
      val valueS: Signal[Option[A]] = self.valueS
      val loadingS: Signal[Boolean] = self.loadingS
      val errorS: Signal[Option[Throwable]] = self.errorS
      // A derived value has no position: its version counts its changes
      private val versionS: Signal[Long] =
        self.valueS.changes.scanLeft(0L)((count, _) => count + 1)
      private val localS: Signal[Local[K, A]] =
        self.valueS
          .combineWith(self.loadingS, keyS, versionS)
          .map((value, loading, key, version) =>
            Local(Some(key), loading, version, value)
          )
      val syncS: Signal[SyncStatus] =
        self.syncS.combineWithFn(
          SyncCheck(feed, name, localS, load, same)
        )(_ ++ _)

end LiveData

object LiveData:

  /** A signal as a live value: always present, never loading, never failing. */
  def fromSignal[A](signal: Signal[A]): LiveData[A] =
    new LiveData[A]:
      val valueS: Signal[Option[A]] = signal.map(Some(_))
      val loadingS: Signal[Boolean] = Val(false)
      val errorS: Signal[Option[Throwable]] = Val(None)
      val syncS: Signal[SyncStatus] = Val(SyncStatus.nothingToCheck)

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
      val syncS: Signal[SyncStatus] = current.flatMapSwitch(_.syncS)
