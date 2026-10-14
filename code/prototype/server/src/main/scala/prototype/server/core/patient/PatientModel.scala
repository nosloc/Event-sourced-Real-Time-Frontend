package prototype.server.core.patient

import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.ErrorCode
import smithy4s.time.LocalDate

object patientModel:
  sealed trait PatientCommand extends Command
  case class CreatePatientCommand(
      patientId: PatientId,
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ) extends PatientCommand

  sealed trait PatientEvent extends Event
  case class PatientCreated(
      patientId: PatientId,
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ) extends PatientEvent

  sealed trait PatientError extends ErrorCode
  case object ALREADY_EXISTS extends PatientError

  type PatientState = Option[Patient]
