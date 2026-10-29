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
