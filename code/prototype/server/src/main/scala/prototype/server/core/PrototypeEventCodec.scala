package prototype.server.core

import prototype.events.PrototypeEvent
import prototype.appointment.AppointmentEvent
import prototype.server.core.company.domain.CompanyEvent
import prototype.server.core.patient.domain.PatientEvent
import prototype.server.core.therapist.domain.TherapistEvent

import persistent4s.EventCodec
import persistent4s.circe.CirceEventCodec

object PrototypeEventCodec:
  val eventCodec: EventCodec[PrototypeEvent] =
    CirceEventCodec
      .builder[PrototypeEvent]
      .add[AppointmentEvent]
      .add[CompanyEvent]
      .add[PatientEvent]
      .add[TherapistEvent]
      .build
