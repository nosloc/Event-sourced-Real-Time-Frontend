package prototype.server.eventfeed

import cats.effect.IO

import prototype.api.{EventStoreService, EventsPage}
import prototype.server.core.PrototypeEvent

import persistent4s.{EventCodec, EventFilter, EventStore}

final class EventStoreServiceImpl(
    eventStore: EventStore[IO, PrototypeEvent],
    codec: EventCodec[PrototypeEvent]
) extends EventStoreService[IO]:

  override def listEvents(skip: Long, take: Int): IO[EventsPage] =
    eventStore
      .readFrom(0L, EventFilter())
      .compile
      .count
      .flatMap(count => getEvents(skip, take, count))
  end listEvents

  override def getLatestEvents(take: Int): IO[EventsPage] =
    eventStore
      .readFrom(0L, EventFilter())
      .compile
      .count
      .flatMap { count =>
        // Bounded first, so that the page ends with the newest event
        val bounded = EventStoreServiceImpl.boundedTake(take)
        getEvents(count - bounded, bounded, count)
      }
  end getLatestEvents

  private def getEvents(skip: Long, take: Int, total: Long): IO[EventsPage] =
    val parsedTake = EventStoreServiceImpl.boundedTake(take)
    val parsedSkip = skip.max(0)
    // P4S takes the number of events to read as an Int
    val toRead = (parsedSkip + parsedTake).min(Int.MaxValue).toInt
    eventStore
      .readFrom(0L, EventFilter(), Some(toRead))
      .drop(parsedSkip)
      .evalMap(envelope => IO.fromEither(EventViews.from(envelope, codec)))
      .compile
      .toList
      .map(events => EventsPage(events, total))
  end getEvents

object EventStoreServiceImpl:

  /** The most events one page can hold, whatever the client asks for */
  val MaxTake: Int = 200

  private def boundedTake(take: Int): Int = take.max(0).min(MaxTake)
