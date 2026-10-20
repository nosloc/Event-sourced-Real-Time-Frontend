package prototype.server.core

import prototype.server.core.company.domain.CompanyEvent

import persistent4s.{Event, EventCodec}
import persistent4s.circe.CirceEventCodec

trait PrototypeEvent extends Event

object PrototypeEvent:

  val eventCodec: EventCodec[PrototypeEvent] =
    CirceEventCodec
      .builder[PrototypeEvent]
      .add[CompanyEvent]
      .build
