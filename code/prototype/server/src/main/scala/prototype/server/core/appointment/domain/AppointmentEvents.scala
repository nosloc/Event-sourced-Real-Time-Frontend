package prototype.server.core.appointment.domain

import java.util.UUID

import prototype.api.PrestationStatus
import prototype.server.core.PrototypeEvent

import io.circe.{Decoder, Encoder}

// The stored JSON is the status name, e.g. "TO_BILL"
given Encoder[PrestationStatus] = Encoder[String].contramap(_.stringValue)
given Decoder[PrestationStatus] = Decoder[String].emap(value =>
  PrestationStatus.values
    .find(_.stringValue == value)
    .toRight(s"Unknown prestation status: $value")
)

sealed trait AppointmentEvent extends PrototypeEvent:

  /** Identifies the appointment that this event is about */
  def id: UUID

final case class AppointmentCreated(
    id: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    startTime: Long,
    endTime: Long
) extends AppointmentEvent derives Encoder, Decoder

// What an update changed, and only that: a field is None when it kept its value,
// and the lists name who joined and who left. The people who join are not told
// by this event: each gets a TherapistAddedToAppointment or a
// PatientAddedToAppointment with the whole appointment, since their stream holds
// nothing to apply a change to.
final case class AppointmentUpdated(
    id: UUID,
    therapistsAdded: List[String],
    therapistsRemoved: List[String],
    patientsAdded: List[UUID],
    patientsRemoved: List[UUID],
    apptType: Option[String],
    startTime: Option[Long],
    endTime: Option[Long]
) extends AppointmentEvent derives Encoder, Decoder

final case class AppointmentDeleted(
    id: UUID
) extends AppointmentEvent derives Encoder, Decoder

final case class PrestationCreated(
    id: UUID, // The appointment id
    prestationId: UUID,
    label: String,
    unitPrice: Float,
    quantity: Int
) extends AppointmentEvent derives Encoder, Decoder

final case class PrestationStatusUpdated(
    id: UUID, // The appointment id
    prestationId: UUID,
    status: PrestationStatus
) extends AppointmentEvent derives Encoder, Decoder

final case class PrestationDeleted(
    id: UUID, // The appointment id
    prestationId: UUID
) extends AppointmentEvent derives Encoder, Decoder

// To each therapist an update adds, and only in their stream: the whole
// appointment as the update leaves it, since their stream holds nothing about
// it to apply the update to. The appointment projection ignores it: the update
// already lists who joined
final case class TherapistAddedToAppointment(
    id: UUID, // The appointment id
    therapistId: String, // The trigram of the therapist added
    appointmentTherapists: List[String],
    appointmentPatients: List[UUID],
    appointmentType: String,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends AppointmentEvent derives Encoder, Decoder

// The same for each patient an update adds
final case class PatientAddedToAppointment(
    id: UUID, // The appointment id
    patientId: UUID, // The patient added
    appointmentTherapists: List[String],
    appointmentPatients: List[UUID],
    appointmentType: String,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends AppointmentEvent derives Encoder, Decoder
