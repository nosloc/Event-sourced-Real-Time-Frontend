package prototype.server.faults

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}

import prototype.api.{DelayMillis, Faults, ReadModel}

/** The faults the server injects on purpose, set from the Faults page. One per
  * server, in memory: a restart clears them. Each injection point reads its
  * fault when it runs (a request, an event), so a change applies to the next
  * one.
  *
  * Off (`enabled` false): no fault can be set, and the injection points add
  * nothing at all, not even a wait of 0.
  */
final class FaultInjection private (
    val enabled: Boolean,
    state: Ref[IO, Faults]
):

  def current: IO[Faults] = state.get

  /** Replaces every fault. */
  def set(faults: Faults): IO[Faults] = state.set(faults).as(faults)

  /** Added after a snapshot was read, before it is sent. */
  def snapshotDelay: IO[FiniteDuration] =
    current.map(_.snapshotDelayMillis.value.millis)

  /** Added before `readModel` applies each event. */
  def readModelDelay(readModel: ReadModel): IO[FiniteDuration] =
    current.map(FaultInjection.readModelDelay(_, readModel))

  /** What a read model runs before each event: its delay, or nothing when off.
    */
  def readModelWait(readModel: ReadModel): IO[Unit] =
    if enabled then readModelDelay(readModel).flatMap(IO.sleep) else IO.unit

object FaultInjection:

  /** No fault at all. */
  val none: Faults = Faults(DelayMillis(0), Nil)

  def make(enabled: Boolean): IO[FaultInjection] =
    Ref.of[IO, Faults](none).map(new FaultInjection(enabled, _))

  /** The delay of one read model: none when it is not listed, the last entry
    * when it is listed twice.
    */
  def readModelDelay(faults: Faults, readModel: ReadModel): FiniteDuration =
    faults.readModelDelays
      .findLast(_.readModel == readModel)
      .fold(0)(_.delayMillis.value)
      .millis
end FaultInjection
