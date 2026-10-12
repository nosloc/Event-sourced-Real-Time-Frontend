package prototype.server.core.therapist

import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.ErrorCode

object therapistModel:
  sealed trait TherapistCommand extends Command
  case class CreateTherapistCommand(
      therapistId: TherapistId,
      firstName: String,
      lastName: String,
      companyId: CompanyId,
      role: Role
  ) extends TherapistCommand

  sealed trait TherapistEvent extends Event
  case class TherapistCreated(
      therapistId: TherapistId,
      firstName: String,
      lastName: String,
      companyId: CompanyId,
      role: Role
  ) extends TherapistEvent

  sealed trait TherapistError extends ErrorCode
  case object ALREADY_EXISTS extends TherapistError

  type TherapistState = Option[Therapist]
