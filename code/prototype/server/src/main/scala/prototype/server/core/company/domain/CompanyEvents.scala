package prototype.server.core.company.domain

import java.util.UUID

import prototype.server.core.PrototypeEvent

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
