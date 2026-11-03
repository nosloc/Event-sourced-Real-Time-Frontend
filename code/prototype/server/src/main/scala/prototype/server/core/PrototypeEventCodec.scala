package prototype.server.core

import prototype.server.core.patient.domain.PatientEvent
import prototype.server.core.therapist.domain.TherapistEvent
import prototype.shared.appointment.AppointmentEvent
import prototype.shared.company.CompanyEvent
import prototype.shared.events.PrototypeEvent

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
