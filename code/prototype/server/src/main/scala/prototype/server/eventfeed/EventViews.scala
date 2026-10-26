package prototype.server.eventfeed

import prototype.api.*
import prototype.server.core.PrototypeEvent

import persistent4s.{EventCodec, EventEnvelope}

import smithy4s.time.Timestamp

object EventViews:
  def from(
      envelope: EventEnvelope[PrototypeEvent],
      codec: EventCodec[PrototypeEvent]
  ): Either[Throwable, EventView] =
    codec
      .encode(envelope.payload)
      .map(payload =>
        EventView(
          position = envelope.metadata.globalPosition,
          eventId = envelope.metadata.id.toString,
          eventType = envelope.metadata.eventType.value,
          recordedAt = Timestamp.fromInstant(envelope.metadata.timestamp),
          tags = envelope.metadata.tags.toList.map(_.value).sorted,
          payload = payload
        )
      )
