package prototype.server.core

import prototype.server.core.company.domain.CompanyEvent
import prototype.server.core.therapist.domain.TherapistEvent

import persistent4s.{Event, EventCodec}
import persistent4s.circe.CirceEventCodec

trait PrototypeEvent extends Event

object PrototypeEvent:

  val eventCodec: EventCodec[PrototypeEvent] =
    CirceEventCodec
      .builder[PrototypeEvent]
      .add[CompanyEvent]
      .add[TherapistEvent]
      .build
