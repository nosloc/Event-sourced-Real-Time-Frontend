package prototype.shared.appointment

import java.util.UUID

import prototype.api.{AppointmentType, EventMessage, PrestationStatus}
import prototype.shared.events.{EventType, PrototypeEvent}

import io.circe.{Decoder, Encoder}

// The stored JSON is the status name, e.g. "TO_BILL"
given Encoder[PrestationStatus] = Encoder[String].contramap(_.stringValue)
given Decoder[PrestationStatus] = Decoder[String].emap(value =>
  PrestationStatus
    .fromString(value)
    .toRight(s"Unknown prestation status: $value")
)

given Encoder[AppointmentType] = Encoder[String].contramap(_.stringValue)
given Decoder[AppointmentType] = Decoder[String].emap(value =>
  AppointmentType.fromString(value).toRight(s"Unknown appointment type: $value")
)

sealed trait AppointmentEvent extends PrototypeEvent:

  /** Identifies the appointment that this event is about */
  def id: UUID

object AppointmentEvent:

  // The decoder of each appointment event, by type name
  private val decoders: EventType.Decoders[AppointmentEvent] =
    Map(
      EventType.decoder[AppointmentCreated],
      EventType.decoder[AppointmentUpdated],
      EventType.decoder[AppointmentDeleted],
      EventType.decoder[PrestationCreated],
      EventType.decoder[PrestationStatusUpdated],
      EventType.decoder[PrestationDeleted],
      EventType.decoder[TherapistAddedToAppointment],
      EventType.decoder[PatientAddedToAppointment]
    )

  /** The appointment event in an event message, read by its type name (P4S
    * stores an event under its class name). None: not an appointment event.
    * Some(Left): an appointment event whose payload does not decode.
    */
  def decode(
      message: EventMessage
  ): Option[Either[io.circe.Error, AppointmentEvent]] =
    decoders.get(message.eventType).map(_(message.payload))
end AppointmentEvent

final case class AppointmentCreated(
    id: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: AppointmentType,
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
    apptType: Option[AppointmentType],
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
    appointmentType: AppointmentType,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends AppointmentEvent derives Encoder, Decoder

// The same for each patient an update adds
final case class PatientAddedToAppointment(
    id: UUID, // The appointment id
    patientId: UUID, // The patient added
    appointmentTherapists: List[String],
    appointmentPatients: List[UUID],
    appointmentType: AppointmentType,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends AppointmentEvent derives Encoder, Decoder
