package prototype.server.core.therapist

import prototype.server.core.therapist.therapistModel.*
import prototype.server.eventStore.*
import prototype.api.*

class TherapistDecider
    extends Decider[
      TherapistState,
      TherapistCommand,
      TherapistEvent,
      TherapistError
    ]:

  def initialState: TherapistState = None

  def decide(
      state: TherapistState,
      command: TherapistCommand
  ): Either[TherapistError, List[TherapistEvent]] =
    command match
      case CreateTherapistCommand(
            therapistId,
            firstName,
            lastName,
            companyId,
            role
          ) =>
        state match
          case None =>
            Right(
              List(
                TherapistCreated(
                  therapistId,
                  firstName,
                  lastName,
                  companyId,
                  role
                )
              )
            )
          case Some(_) =>
            Left(ALREADY_EXISTS)

  def evolve(
      state: TherapistState,
      event: TherapistEvent
  ): TherapistState =
    event match
      case TherapistCreated(therapistId, firstName, lastName, companyId, role) =>
        Some(Therapist(firstName, lastName, therapistId, companyId, role))
