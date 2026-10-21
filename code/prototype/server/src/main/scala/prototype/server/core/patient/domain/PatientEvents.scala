package prototype.server.core.patient.domain

import java.time.LocalDate
import java.util.UUID

import prototype.server.core.PrototypeEvent

import io.circe.{Decoder, Encoder}

sealed trait PatientEvent extends PrototypeEvent:
  def id: UUID

final case class PatientCreated(
    id: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
) extends PatientEvent derives Encoder, Decoder
