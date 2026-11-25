package prototype.server.eventfeed

import prototype.api.*
import prototype.server.eventfeed.ApiCursor.toApi
import prototype.shared.events.PrototypeEvent

import persistent4s.{EventCodec, EventEnvelope}

import smithy4s.time.Timestamp

object EventMessages:
  def from(
      envelope: EventEnvelope[PrototypeEvent],
      codec: EventCodec[PrototypeEvent]
  ): Either[Throwable, EventMessage] =
    codec
      .encode(envelope.payload)
      .map(payload =>
        EventMessage(
          cursor = envelope.metadata.cursor.toApi,
          eventId = envelope.metadata.id.toString,
          eventType = envelope.metadata.eventType.value,
          recordedAt = Timestamp.fromInstant(envelope.metadata.timestamp),
          tags = envelope.metadata.tags.toList.map(_.value).sorted,
          payload = payload
        )
      )
