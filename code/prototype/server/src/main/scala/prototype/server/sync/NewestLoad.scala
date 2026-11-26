package prototype.server.sync

import cats.effect.IO
import cats.syntax.all.*

import prototype.api.{CatchUp, Cursor}
import prototype.server.eventfeed.ApiCursor.toApi
import prototype.server.eventfeed.EventMessages
import prototype.shared.events.PrototypeEvent

import persistent4s.{
  EventCodec,
  EventEnvelope,
  EventFilter,
  EventStore,
  EventTypeName,
  ReadCursor
}

/** A read model's name and the event types it handles. */
final case class ReadModel(name: String, eventTypes: Set[EventTypeName])

/** The result of a load: a snapshot, the read model's checkpoint, and the
  * catch-up events.
  */
final case class Loaded[A](snapshot: A, cursor: Cursor, catchUp: CatchUp)

/** The read model lags by more events than a load sends. */
final case class TooFarBehind(readModel: String, limit: Int)
    extends RuntimeException(
      s"The $readModel read model is more than $limit events behind"
    )

/** Loads a snapshot with the events its read model has not saved yet: every
  * event of its types after its checkpoint. The head is the last of them, or
  * the checkpoint. Over `maxEvents`, the load fails.
  *
  * @param eventStore
  *   where the catch-up is read from
  * @param codec
  *   turns the catch-up's events into the API's messages
  * @param pageSize
  *   how many events are read from the store at a time
  * @param maxEvents
  *   how many events a catch-up may hold at most
  */
final class NewestLoad(
    eventStore: EventStore[IO, PrototypeEvent],
    codec: EventCodec[PrototypeEvent],
    pageSize: Int = NewestLoad.PageSize,
    maxEvents: Int = NewestLoad.MaxEvents
):

  /** Reads the snapshot, then its catch-up.
    *
    * @param readModel
    *   the read model the snapshot is read from
    * @param snapshot
    *   reads the snapshot and the read model's checkpoint, in one transaction
    *   (`readWithCursor`)
    * @return
    *   the snapshot, its checkpoint and its catch-up; fails with `TooFarBehind`
    *   when the catch-up holds more than `maxEvents` events
    */
  def apply[A](readModel: ReadModel)(
      snapshot: IO[(A, ReadCursor)]
  ): IO[Loaded[A]] =
    for
      (value, checkpoint) <- snapshot
      events <- catchUp(readModel, checkpoint)
      messages <- events.traverse(e =>
        IO.fromEither(EventMessages.from(e, codec))
      )
      head = events.lastOption.fold(checkpoint)(_.metadata.cursor)
    yield Loaded(value, checkpoint.toApi, CatchUp(messages, head.toApi))

  /** Reads every event of the read model's types after the checkpoint, page by
    * page, in delivery order.
    *
    * @param readModel
    *   gives the event types to read, and its name for the error
    * @param checkpoint
    *   the read model's checkpoint: the catch-up starts after it
    * @return
    *   the events, oldest first; fails with `TooFarBehind` over `maxEvents`
    */
  private def catchUp(
      readModel: ReadModel,
      checkpoint: ReadCursor
  ): IO[List[EventEnvelope[PrototypeEvent]]] =
    val filter = EventFilter(eventTypes = readModel.eventTypes)
    // A page shorter than asked for means everything deliverable was read
    def from(
        after: ReadCursor,
        read: Vector[EventEnvelope[PrototypeEvent]]
    ): IO[Vector[EventEnvelope[PrototypeEvent]]] =
      eventStore.readForProjection(after, filter, pageSize).flatMap { page =>
        val all = read ++ page.events
        if all.size > maxEvents then
          IO.raiseError(TooFarBehind(readModel.name, maxEvents))
        else if page.events.size < pageSize then IO.pure(all)
        else from(page.events.last.metadata.cursor, all)
      }
    from(checkpoint, Vector.empty).map(_.toList)

object NewestLoad:

  /** Events read from the store at a time */
  val PageSize: Int = 500

  /** Events a load sends at most; beyond, it fails with `TooFarBehind` */
  val MaxEvents: Int = 1000
