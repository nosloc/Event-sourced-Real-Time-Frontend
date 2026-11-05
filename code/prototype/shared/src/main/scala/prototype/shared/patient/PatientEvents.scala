package prototype.shared.patient

import java.time.LocalDate
import java.util.UUID

import prototype.api.EventMessage
import prototype.shared.events.{EventType, PrototypeEvent}

import io.circe.{Decoder, Encoder}

sealed trait PatientEvent extends PrototypeEvent:
  def id: UUID

final case class PatientCreated(
    id: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
) extends PatientEvent derives Encoder, Decoder

object PatientEvent:

  // The decoder of each patient event, by type name
  private val decoders: EventType.Decoders[PatientEvent] =
    Map(
      EventType.decoder[PatientCreated]
    )

  /** The patient event in an event message, read by its type name (P4S stores
    * an event under its class name). None: not a patient event. Some(Left): a
    * patient event whose payload does not decode.
    */
  def decode(
      message: EventMessage
  ): Option[Either[io.circe.Error, PatientEvent]] =
    decoders.get(message.eventType).map(_(message.payload))
