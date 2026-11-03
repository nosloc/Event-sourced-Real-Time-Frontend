package prototype.shared.company

import java.util.UUID

import prototype.api.EventMessage
import prototype.shared.events.PrototypeEvent

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

  /** The company event in an event message, read by its type name (P4S stores
    * an event under its class name). None: not a company event. Some(Left): a
    * company event whose payload does not decode.
    */
  def decode(
      message: EventMessage
  ): Option[Either[io.circe.Error, CompanyEvent]] =
    message.eventType match
      case "CompanyCreated" =>
        Some(io.circe.parser.decode[CompanyCreated](message.payload))
      case _ => None
