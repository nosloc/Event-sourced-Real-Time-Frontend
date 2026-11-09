package prototype.shared.company

import java.util.UUID

import prototype.api.EventMessage
import prototype.shared.events.{EventType, PrototypeEvent}

import io.circe.{Decoder, Encoder}

sealed trait CompanyEvent extends PrototypeEvent:

  /** Identifies the company that this event is about
    *
    * @return
    *   company id
    */
  def id: UUID

/** Records a new company after duplication check has been done.
  *
  * @param id
  *   company id
  * @param name
  *   company name
  * @param addressStreet
  *   company address street
  * @param addressNumber
  *   company address number
  * @param addressCity
  *   company address city
  * @param addressState
  *   company address state
  */
final case class CompanyCreated(
    id: UUID,
    name: String,
    addressStreet: String,
    addressNumber: String,
    addressCity: String,
    addressState: String
) extends CompanyEvent derives Encoder, Decoder

object CompanyEvent:

  // The decoder of each company event, by type name
  private val decoders: EventType.Decoders[CompanyEvent] =
    Map(
      EventType.decoder[CompanyCreated]
    )

  /** The company event in an event message, read by its type name (P4S stores
    * an event under its class name).
    *
    * @param message
    *   the event message to decode
    * @return
    *   None: not a company event. Some(Left): a company event whose payload
    *   does not decode. Some(Right): a company event whose payload decodes
    */
  def decode(
      message: EventMessage
  ): Option[Either[io.circe.Error, CompanyEvent]] =
    decoders.get(message.eventType).map(_(message.payload))
