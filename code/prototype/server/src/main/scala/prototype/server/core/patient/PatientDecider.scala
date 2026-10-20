package prototype.server.core.patient

import prototype.api.*
import prototype.server.core.patient.patientModel.*
import prototype.server.eventStore.*

class PatientDecider
    extends Decider[
      PatientState,
      PatientCommand,
      PatientEvent,
      PatientError
    ]:

  override def initialState: PatientState = None

  override def decide(
      state: PatientState,
      command: PatientCommand
  ): Either[PatientError, List[PatientEvent]] =
    command match
      case CreatePatientCommand(patientId, firstName, lastName, dob) =>
        state match
          case None =>
            Right(List(PatientCreated(patientId, firstName, lastName, dob)))
          case Some(_) =>
            Left(ALREADY_EXISTS)

  override def evolve(
      state: PatientState,
      event: PatientEvent
  ): PatientState =
    event match
      case PatientCreated(patientId, firstName, lastName, dob) =>
        Some(Patient(patientId, firstName, lastName, dob))
