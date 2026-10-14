package prototype.server.core.patient

import cats.effect.*
import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.core.patient.patientModel.*
import smithy4s.time.LocalDate
import java.util.UUID

trait PatientServiceImpl extends PatientService[IO]

object PatientServiceImpl:
  def apply(
      eventStore: EventStore
  ): IO[PatientServiceImpl] =
    for
      view <- Ref.of[IO, Map[String, Patient]](Map.empty)
      viewer = PatientViewer(view)
      decider = PatientDecider()

    yield new PatientServiceImpl:

      val handler = new CommandHandler(decider, viewer, eventStore)

      override def createPatient(
          firstName: String,
          lastName: String,
          dateOfBirth: LocalDate
      ): IO[CreatePatientOutput] =
        for
          alreadyExists <- viewer.alreadyExists(
            firstName,
            lastName,
            dateOfBirth
          )
          _ <-
            if alreadyExists then IO.raiseError(PatientAlreadyExists())
            else IO.unit
          id = PatientId(UUID.randomUUID())
          command = CreatePatientCommand(
            id,
            firstName,
            lastName,
            dateOfBirth
          )
          _ <- IO.print(
            s"Creating patient $id: $firstName $lastName, DOB: $dateOfBirth\n"
          )
          result <- handler.handle(id.value.toString, command)
          returnValue <- result match
            case Right(patient) => IO.pure(CreatePatientOutput(patient.get.id))
            case Left(ALREADY_EXISTS) =>
              IO.raiseError(
                PatientAlreadyExists() // This should not happen because we already checked for existence and generated a new ID, but we handle it just in case.
              )
        yield returnValue

      override def listPatients(): IO[ListPatientsOutput] =
        viewer
          .getAllPatients()
          .map(ListPatientsOutput.apply)

      override def getPatient(patientId: PatientId): IO[GetPatientOutput] =
        viewer.getState(patientId.value.toString).flatMap {
          case Some(patient) => IO.pure(GetPatientOutput(patient))
          case None          => IO.raiseError(PatientNotFound())
        }
