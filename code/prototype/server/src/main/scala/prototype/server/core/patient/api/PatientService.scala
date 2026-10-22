package prototype.server.core.patient.api

import cats.effect.IO

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.patient.commands.CreatePatient
import prototype.server.core.patient.domain.PatientError
import prototype.server.core.patient.domain.PatientError.*
import prototype.server.core.patient.persistence.{
  PatientRepository,
  PatientState
}

import persistent4s.CommandRuntime

import org.typelevel.log4cats.slf4j.Slf4jLogger
import smithy4s.time.LocalDate

class PatientServiceImpl(
    patients: PatientRepository
)(using CommandRuntime[IO, PrototypeEvent])
    extends PatientService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  private def fromState(state: PatientState): Patient =
    Patient(
      PatientId(state.patientId),
      state.firstName,
      state.lastName,
      LocalDate.fromJava(state.dateOfBirth)
    )

  // The id is generated here, like the company's. Names are not logged: they
  // identify a person.
  override def createPatient(
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ): IO[CreatePatientOutput] =
    for
      id <- IO.randomUUID
      _ <- CreatePatient.Handler.runOrRaise[IO](
        CreatePatient(
          id,
          firstName,
          lastName,
          dateOfBirth.toJava
        )
      ) {
        case PATIENT_ALREADY_EXISTS(_) => PatientAlreadyExists()
        case other: PatientError       => other
      }
      _ <- logger.info(s"Created patient $id")
    yield CreatePatientOutput(PatientId(id))

  // Sorted by last then first name: a projection has no stable order
  override def listPatients(): IO[ListPatientsOutput] =
    patients.all.map(all =>
      ListPatientsOutput(
        all.sortBy(p => (p.lastName, p.firstName)).map(fromState)
      )
    )

  override def getPatient(patientId: PatientId): IO[GetPatientOutput] =
    patients.find(patientId.value).flatMap {
      case Some(state) => IO.pure(GetPatientOutput(fromState(state)))
      case None        => IO.raiseError(PatientNotFound(patientId))
    }
