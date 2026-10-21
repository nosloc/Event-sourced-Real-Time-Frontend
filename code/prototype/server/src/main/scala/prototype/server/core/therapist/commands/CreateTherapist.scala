package prototype.server.core.therapist.commands

import java.util.UUID

import prototype.api.Role
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.domain.{CompanyCreated, CompanyScopes}
import prototype.server.core.therapist.domain.{
  TherapistCreated,
  TherapistError,
  TherapistScopes
}
import prototype.server.core.therapist.domain.TherapistError.*

import persistent4s.EventSourcedCommandHandler

final case class CreateTherapist(
    therapistId: String,
    firstName: String,
    lastName: String,
    companyId: UUID,
    role: Role
)

object CreateTherapist:

  /** What the handler needs to know: the therapist id is taken, and the company
    * exists (read from the company's own stream).
    */
  final case class State(therapistExists: Boolean, companyExists: Boolean)

  object Handler
      extends EventSourcedCommandHandler[
        CreateTherapist,
        State,
        PrototypeEvent,
        TherapistError
      ]:

    override protected val behavior = handler(initial = State(false, false)):
      scope(TherapistScopes.Therapist)(_.therapistId)
      scope(CompanyScopes.Company)(_.companyId)

      on[TherapistCreated].allEvents
        .evolve((state, _) => state.copy(therapistExists = true))
      on[CompanyCreated].allEvents.evolve((state, _) =>
        state.copy(companyExists = true)
      )

      reject:
        case (state, command) if state.therapistExists =>
          THERAPIST_ALREADY_EXISTS(command.therapistId)
        case (state, command) if !state.companyExists =>
          THERAPIST_COMPANY_NOT_FOUND(command.therapistId)
        case (_, command)
            if command.firstName.trim.isEmpty || command.lastName.trim.isEmpty =>
          THERAPIST_NAME_EMPTY(command.therapistId)

      emitTagged: command =>
        (
          Set(TherapistScopes.Therapist(command.therapistId).toTag),
          TherapistCreated(
            command.therapistId,
            command.firstName,
            command.lastName,
            command.companyId,
            command.role
          )
        )
