package prototype.server.core

import prototype.shared.appointment.AppointmentEvent
import prototype.shared.company.CompanyEvent
import prototype.shared.events.PrototypeEvent
import prototype.shared.patient.PatientEvent
import prototype.shared.therapist.TherapistEvent

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
