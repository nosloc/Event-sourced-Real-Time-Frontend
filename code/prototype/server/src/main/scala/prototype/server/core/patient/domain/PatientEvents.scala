package prototype.server.core.patient.domain

import java.time.LocalDate
import java.util.UUID

import prototype.server.core.PrototypeEvent

import io.circe.{Decoder, Encoder}

sealed trait PatientEvent extends PrototypeEvent:
  def id: UUID

final case class PatientCreated(
    id: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
) extends PatientEvent derives Encoder, Decoder

// Event tagged only on the patient tag used to give all the appointment's information to the patient's stream
final case class PatientAddedToAppointment(
    id: UUID, // The id of the patient added
    appointmentId: UUID,
    appointmentTherapists: List[String],
    appointmentPatients: List[UUID],
    appointmentType: String,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends PatientEvent derives Encoder, Decoder
