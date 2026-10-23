package prototype.server.core.therapist.domain

import java.util.UUID

import prototype.api.Role
import prototype.server.core.PrototypeEvent

import io.circe.{Decoder, Encoder}

// The stored JSON is the role's name, e.g. "PHYSICIAN"
given Encoder[Role] = Encoder[String].contramap(_.stringValue)
given Decoder[Role] = Decoder[String].emap(value =>
  Role.values.find(_.stringValue == value).toRight(s"Unknown role: $value")
)

sealed trait TherapistEvent extends PrototypeEvent:

  def id: String

final case class TherapistCreated(
    id: String, // therapist trigram
    firstName: String,
    lastName: String,
    companyId: UUID,
    role: Role
) extends TherapistEvent derives Encoder, Decoder

// Event tagged only on the therapist tag used to give all the appointment's information to the therapist's stream
final case class TherapistAddedToAppointment(
    id: String, // The trigram of the therapist added
    appointmentId: UUID,
    appointmentTherapists: List[String],
    appointmentPatients: List[UUID],
    appointmentType: String,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends TherapistEvent derives Encoder, Decoder
