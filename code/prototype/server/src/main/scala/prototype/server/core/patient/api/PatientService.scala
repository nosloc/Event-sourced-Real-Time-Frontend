package prototype.server.core.patient.api

import cats.effect.IO

import prototype.api.*
import prototype.server.core.patient.commands.CreatePatient
import prototype.server.core.patient.domain.PatientError.*
import prototype.server.core.patient.persistence.{
  PatientProjection,
  PatientProjectionName,
  PatientRepository
}
import prototype.server.eventfeed.ApiCursor
import prototype.server.sync.{NewestLoad, ReadModel}
import prototype.shared.events.PrototypeEvent
import prototype.shared.patient.PatientState

import persistent4s.CommandRuntime

import org.typelevel.log4cats.slf4j.Slf4jLogger
import smithy4s.time.LocalDate

class PatientServiceImpl(
    patients: PatientRepository,
    load: NewestLoad
)(using CommandRuntime[IO, PrototypeEvent])
    extends PatientService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  private val readModel =
    ReadModel(PatientProjectionName, PatientProjection(patients).filter)

  // The id is generated here, like the company's. Names are not logged: they
  // identify a person.
  override def createPatient(
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ): IO[CreatePatientOutput] =
    for
      id <- IO.randomUUID
      events <- CreatePatient.Handler.runOrRaise[IO](
        CreatePatient(
          id,
          firstName,
          lastName,
          dateOfBirth.toJava
        )
      ) {
        case PATIENT_ALREADY_EXISTS(_)    => PatientAlreadyExists()
        case EMPTY_PATIENT_FIELDS(fields) => InvalidPatient(fields)
      }
      _ <- logger.info(s"Created patient $id")
      cursor <- ApiCursor.required(ApiCursor.lastWritten(events))
    yield CreatePatientOutput(PatientId(id), cursor)

  // Sorted by last then first name: a projection has no stable order
  override def listPatients(): IO[ListPatientsOutput] =
    load(readModel)(patients.allPatients).map(loaded =>
      ListPatientsOutput(
        loaded.snapshot
          .sortBy(p => (p.lastName, p.firstName))
          .map(PatientState.toPatient),
        loaded.cursor,
        loaded.catchUp
      )
    )

  override def getPatient(patientId: PatientId): IO[GetPatientOutput] =
    patients.find(patientId.value).flatMap {
      case Some(state) => IO.pure(GetPatientOutput(PatientState.toPatient(state)))
      case None        => IO.raiseError(PatientNotFound(patientId))
    }
