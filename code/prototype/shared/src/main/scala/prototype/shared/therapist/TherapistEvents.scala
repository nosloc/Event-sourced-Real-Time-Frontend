package prototype.shared.therapist

import java.util.UUID

import prototype.api.{
  EventMessage,
  Role
}
import prototype.shared.events.{EventType, PrototypeEvent}

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

object TherapistEvent:

  // The decoder of each therapist event, by type name
  private val decoders: EventType.Decoders[TherapistEvent] =
    Map(
      EventType.decoder[TherapistCreated]
    )

  /** The therapist event in an event message, read by its type name (P4S stores
    * an event under its class name). None: not a therapist event. Some(Left): a
    * therapist event whose payload does not decode.
    */
  def decode(
      message: EventMessage
  ): Option[Either[io.circe.Error, TherapistEvent]] =
    decoders.get(message.eventType).map(_(message.payload))
