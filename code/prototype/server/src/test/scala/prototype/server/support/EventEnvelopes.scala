package prototype.server.support

import java.time.Instant
import java.util.UUID

import persistent4s.{
  Event,
  EventEnvelope,
  EventMetadata,
  EventTypeName,
  Tag
}

object EventEnvelopes:

  /** An envelope as the store would hand it to a projection. Only the payload
    * and the tags matter to the tests, the rest is filled with fixed values.
    */
  def of[A <: Event](event: A, tags: Set[Tag], eventType: EventTypeName): EventEnvelope[A] =
    EventEnvelope(
      EventMetadata(
        globalPosition = 1L,
        id = new UUID(0L, 0L),
        tags = tags,
        eventType = eventType,
        isExternal = false,
        timestamp = Instant.EPOCH,
        headers = Map.empty
      ),
      event
    )
