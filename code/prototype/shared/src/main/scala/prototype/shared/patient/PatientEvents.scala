package prototype.shared.patient

import java.time.LocalDate
import java.util.UUID

import prototype.api.EventMessage
import prototype.shared.events.PrototypeEvent

import io.circe.{parser, Decoder, Encoder}

sealed trait PatientEvent extends PrototypeEvent:
  def id: UUID

final case class PatientCreated(
    id: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
) extends PatientEvent derives Encoder, Decoder

object PatientEvent:

  /** The patient event in an event message, read by its type name (P4S stores
    * an event under its class name). None: not a patient event. Some(Left): a
    * patient event whose payload does not decode.
    */
  def decode(
      message: EventMessage
  ): Option[Either[io.circe.Error, PatientEvent]] =
    message.eventType match
      case "PatientCreated" =>
        Some(parser.decode[PatientCreated](message.payload))
      case _ => None
