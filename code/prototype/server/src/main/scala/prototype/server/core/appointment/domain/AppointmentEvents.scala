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

// Carries the full new values, not only what changed: the event is also read by
// the streams of the therapists and patients it names, and a stream that never
// saw the creation (someone added by this update) has nothing to apply a
// partial change to.
final case class AppointmentUpdated(
    id: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    startTime: Long,
    endTime: Long
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
