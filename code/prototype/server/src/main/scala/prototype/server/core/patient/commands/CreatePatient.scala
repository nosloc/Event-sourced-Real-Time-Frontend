package prototype.server.core.patient.commands

import java.time.LocalDate
import java.util.UUID

import prototype.server.core.PrototypeEvent
import prototype.server.core.patient.domain.{
  PatientCreated,
  PatientError,
  PatientScopes
}
import prototype.server.core.patient.domain.PatientError.*

import persistent4s.EventSourcedCommandHandler

final case class CreatePatient(
    id: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
)

object CreatePatient:

  private def emptyFields(command: CreatePatient): List[String] =
    List(
      "firstName" -> command.firstName,
      "lastName" -> command.lastName
    ).collect { case (name, value) if value.trim.isEmpty => name }

  object Handler
      extends EventSourcedCommandHandler[
        CreatePatient,
        Boolean,
        PrototypeEvent,
        PatientError
      ]:

    override protected val behavior = handler(initial = false):
      scope(PatientScopes.Patient)(_.id)
      scope(PatientScopes.PatientUnique)(_ => ())

      on[PatientCreated].allEvents.evolve((exists, command, event) =>
        exists ||
          event.id == command.id ||
          (event.firstName == command.firstName &&
            event.lastName == command.lastName &&
            event.dateOfBirth == command.dateOfBirth)
      )

      reject:
        case (_, command) if emptyFields(command).nonEmpty =>
          EMPTY_PATIENT_FIELDS(emptyFields(command))
        case (true, command) =>
          PATIENT_ALREADY_EXISTS(command.id)

      emit: command =>
        PatientCreated(
          command.id,
          command.firstName,
          command.lastName,
          command.dateOfBirth
        )
