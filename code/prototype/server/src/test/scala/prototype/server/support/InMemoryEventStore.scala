package prototype.server.support

import java.util.UUID

import cats.effect.{IO, Ref}
import cats.syntax.all.*

import fs2.Stream

import persistent4s.{
  Event,
  EventEnvelope,
  EventFilter,
  EventMetadata,
  EventStore,
  IndexConflictException,
  PendingEvent
}

final class InMemoryEventStore[A <: Event] private (
    events: Ref[IO, Vector[EventEnvelope[A]]]
) extends EventStore[IO, A]:

  def all: IO[Vector[EventEnvelope[A]]] = events.get

  override def currentRevision(eventFilter: EventFilter): IO[Long] =
    events.get.map(revision(_, eventFilter))

  override def append(
      eventFilter: EventFilter,
      expectedIndex: Long,
      pending: List[PendingEvent[A]]*
  ): IO[List[EventEnvelope[A]]] =
    appendInternal(Some((eventFilter, expectedIndex)), pending.flatten.toList)

  override def appendUnchecked(
      pending: List[PendingEvent[A]]*
  ): IO[List[EventEnvelope[A]]] =
    appendInternal(None, pending.flatten.toList)

  override def readFrom(
      fromPosition: Long,
      eventFilter: EventFilter,
      maxEvents: Option[Int]
  ): Stream[IO, EventEnvelope[A]] =
    val matching = Stream
      .evals(events.get)
      .filter(event =>
        event.metadata.globalPosition > fromPosition && matches(
          event,
          eventFilter
        )
      )
    maxEvents.fold(matching)(max => matching.take(max.toLong))

  private def revision(
      current: Vector[EventEnvelope[A]],
      eventFilter: EventFilter
  ): Long =
    current
      .filter(matches(_, eventFilter))
      .lastOption
      .fold(0L)(_.metadata.globalPosition)

  private def matches(
      event: EventEnvelope[A],
      eventFilter: EventFilter
  ): Boolean =
    (eventFilter.tags.isEmpty || event.metadata.tags.exists(
      eventFilter.tags.contains
    )) &&
      (eventFilter.eventTypes.isEmpty || eventFilter.eventTypes.contains(
        event.metadata.eventType
      ))

  private def appendInternal(
      check: Option[(EventFilter, Long)],
      pending: List[PendingEvent[A]]
  ): IO[List[EventEnvelope[A]]] =
    if pending.isEmpty then IO.pure(List.empty)
    else
      for
        now <- IO.realTimeInstant
        ids <- pending.traverse(_.id.fold(IO.delay(UUID.randomUUID()))(IO.pure))
        result <- events.modify { current =>
          val conflict = check.flatMap { (filter, expectedIndex) =>
            val actualIndex = revision(current, filter)
            Option.when(actualIndex != expectedIndex)(
              IndexConflictException(expectedIndex, actualIndex)
            )
          }
          conflict match
            case Some(error) => (current, Left(error))
            case None        =>
              val existing =
                current.map(event => event.metadata.id -> event).toMap
              val lastPosition =
                current.lastOption.fold(0L)(_.metadata.globalPosition)
              val (written, _, appended) = pending
                .zip(ids)
                .foldLeft(
                  (
                    Vector.empty[EventEnvelope[A]],
                    existing,
                    List.empty[EventEnvelope[A]]
                  )
                ) { case ((written, byId, appended), (event, id)) =>
                  byId.get(id) match
                    case Some(envelope) => (written, byId, envelope :: appended)
                    case None           =>
                      val envelope = EventEnvelope(
                        EventMetadata(
                          globalPosition = lastPosition + written.size + 1,
                          id = id,
                          tags = event.tags,
                          eventType = event.eventType,
                          isExternal = event.isExternal,
                          timestamp = now,
                          headers = event.headers
                        ),
                        event.payload
                      )
                      (
                        written :+ envelope,
                        byId.updated(id, envelope),
                        envelope :: appended
                      )
                }
              (current ++ written, Right(appended.reverse))
        }
        appended <- IO.fromEither(result)
      yield appended

object InMemoryEventStore:

  def make[A <: Event]: IO[InMemoryEventStore[A]] =
    Ref
      .of[IO, Vector[EventEnvelope[A]]](Vector.empty)
      .map(new InMemoryEventStore(_))
