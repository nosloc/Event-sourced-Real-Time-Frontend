package prototype.server.core.appointment

import java.util.UUID

import cats.effect.*

import prototype.api.*
import prototype.server.UserFacingError
import prototype.server.core.appointment.appointmentModel.*
import prototype.server.core.patient.PatientServiceImpl
import prototype.server.core.therapist.TherapistServiceImpl
import prototype.server.eventStore.*

trait AppointmentServiceImpl extends AppointmentService[IO]

object AppointmentServiceImpl:
  def apply(
      eventStore: EventStore,
      therapistService: TherapistServiceImpl,
      patientService: PatientServiceImpl
  ): IO[AppointmentServiceImpl] =
    for
      view <- Ref.of[IO, Map[String, Appointment]](Map.empty)
      viewer = AppointmentViewer(view)
      decider = AppointmentDecider()
    yield new AppointmentServiceImpl:

      val handler = new CommandHandler(decider, viewer, eventStore)

      // Fails with TherapistNotFound if the therapist does not exist
      override def listTherapistAppointments(
          therapistId: TherapistId
      ): IO[ListTherapistAppointmentsOutput] =
        therapistService.getTherapist(therapistId).flatMap { _ =>
          viewer
            .listAppointments(therapistId)
            .map(ListTherapistAppointmentsOutput.apply)
        }
      end listTherapistAppointments

      override def createAppointment(
          therapistIds: List[TherapistId],
          patientIds: List[PatientId],
          apptType: AppointmentType,
          timeRange: TimeRange
      ): IO[CreateAppointmentOutput] =
        val id = UUID.randomUUID()
        val command = CreateAppointmentCommand(
          AppointmentId(id),
          therapistIds,
          patientIds,
          apptType,
          timeRange
        )
        // Fails with TherapistsNotFound or PatientsNotFound if a participant
        // does not exist, and with TherapistBusy or PatientBusy if a
        // participant already has an appointment at that time
        for
          _ <- verifyTherapists(therapistIds, timeRange, ignoring = None)
          _ <- verifyPatients(patientIds, timeRange, ignoring = None)
          result <- handler.handle(id.toString, command).flatMap {
            case Right(appt) => IO.pure(CreateAppointmentOutput(appt.get))
            case Left(INVALID_TIME_RANGE) =>
              IO.raiseError(InvalidTimeRange())
            case Left(NO_THERAPIST) => IO.raiseError(InvalidParticipants())
            case Left(error)        => IO.raiseError(UserFacingError(error))
          }
        yield (result)
      end createAppointment

      override def deleteAppointment(
          appointmentId: AppointmentId
      ): IO[Unit] =
        val command = DeleteAppointmentCommand(appointmentId)
        handler.handle(appointmentId.value.toString, command).flatMap {
          case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
          case Left(error)     => IO.raiseError(UserFacingError(error))
          case Right(_)        => IO.unit
        }
      end deleteAppointment

      // The update sets the type, the participants and the time: the new
      // participants must exist and be free in the new range
      override def updateAppointment(
          appointmentId: AppointmentId,
          apptType: AppointmentType,
          therapistIds: List[TherapistId],
          patientIds: List[PatientId],
          timeRange: TimeRange
      ): IO[UpdateAppointmentOutput] =
        viewer.getState(appointmentId.value.toString).flatMap {
          case None    => IO.raiseError(AppointmentNotFound())
          case Some(_) =>
            val command = UpdateAppointmentCommand(
              appointmentId,
              apptType = apptType,
              timeRange,
              therapistIds = therapistIds,
              patientIds = patientIds
            )
            for
              _ <- verifyTherapists(
                therapistIds,
                timeRange,
                ignoring = Some(appointmentId)
              )
              _ <- verifyPatients(
                patientIds,
                timeRange,
                ignoring = Some(appointmentId)
              )
              result <-
                handler.handle(appointmentId.value.toString, command).flatMap {
                  case Right(appt) =>
                    IO.pure(UpdateAppointmentOutput(appt.get))
                  case Left(INVALID_TIME_RANGE) =>
                    IO.raiseError(InvalidTimeRange())
                  case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
                  case Left(NO_THERAPIST) =>
                    IO.raiseError(InvalidParticipants())
                  case Left(error) => IO.raiseError(UserFacingError(error))
                }
            yield (result)
        }
      end updateAppointment

      override def createPrestation(
          appointmentId: AppointmentId,
          label: String,
          quantity: Int,
          unitPrice: Float
      ): IO[CreatePrestationOutput] =
        val prestationId = PrestationId(UUID.randomUUID())
        val command = CreatePrestationCommand(
          appointmentId,
          prestationId,
          label,
          unitPrice,
          quantity
        )
        handler.handle(appointmentId.value.toString, command).flatMap {
          case Right(appt) =>
            IO.pure(CreatePrestationOutput(appt.get))
          case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
          case Left(PRESTATION_NOT_FOUND) =>
            IO.raiseError(PrestationNotFound())
          case Left(INVALID_PRESTATION) => IO.raiseError(InvalidPrestation())
          case Left(error)              => IO.raiseError(UserFacingError(error))
        }
      end createPrestation

      override def updatePrestationStatus(
          appointmentId: AppointmentId,
          prestationId: PrestationId,
          status: PrestationStatus
      ): IO[UpdatePrestationStatusOutput] =
        val command = UpdatePrestationStatusCommand(
          appointmentId,
          prestationId,
          status
        )
        handler.handle(appointmentId.value.toString, command).flatMap {
          case Right(appt) =>
            IO.pure(UpdatePrestationStatusOutput(appt.get))
          case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
          case Left(PRESTATION_NOT_FOUND) =>
            IO.raiseError(PrestationNotFound())
          case Left(INVALID_STATUS_CHANGE) =>
            IO.raiseError(InvalidStatusChange())
          case Left(error) => IO.raiseError(UserFacingError(error))
        }
      end updatePrestationStatus

      override def deletePrestation(
          appointmentId: AppointmentId,
          prestationId: PrestationId
      ): IO[Unit] =
        val command = DeletePrestationCommand(appointmentId, prestationId)
        handler.handle(appointmentId.value.toString, command).flatMap {
          case Right(_) =>
            IO.unit
          case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
          case Left(PRESTATION_NOT_FOUND) =>
            IO.raiseError(PrestationNotFound())
          case Left(error) => IO.raiseError(UserFacingError(error))
        }
      end deletePrestation

      override def getAppointment(
          appointmentId: AppointmentId
      ): IO[GetAppointmentOutput] =
        viewer.getState(appointmentId.value.toString).flatMap {
          case Some(appt) => IO.pure(GetAppointmentOutput(appt))
          case None       => IO.raiseError(AppointmentNotFound())
        }

      private def verifyTherapists(
          therapistIds: List[TherapistId],
          timeRange: TimeRange,
          ignoring: Option[AppointmentId]
      ): IO[Unit] =
        for
          therapists <- therapistService.getTherapists(therapistIds)
          _ <-
            if therapists.size != therapistIds.distinct.size then
              IO.raiseError(
                TherapistsNotFound(
                  therapistIds.distinct diff therapists.map(_.therapistId)
                )
              )
            else IO.unit
          _ <- failIfOverlappingForTherapists(
            therapistIds,
            timeRange,
            ignoring = ignoring
          )
        yield ()

      private def verifyPatients(
          patientIds: List[PatientId],
          timeRange: TimeRange,
          ignoring: Option[AppointmentId]
      ): IO[Unit] =
        for
          patients <- patientService.getPatients(patientIds)
          _ <-
            if patients.size != patientIds.distinct.size then
              IO.raiseError(
                PatientsNotFound(
                  patientIds.distinct diff patients.map(_.id)
                )
              )
            else IO.unit
          _ <- failIfOverlappingForPatients(
            patientIds,
            timeRange,
            ignoring = ignoring
          )
        yield ()

      // Fails with TherapistBusy, carrying the therapists who are busy
      private def failIfOverlappingForTherapists(
          therapistIds: List[TherapistId],
          timeRange: TimeRange,
          ignoring: Option[AppointmentId]
      ): IO[Unit] =
        if !isValidRange(timeRange) then IO.unit
        else
          viewer.listAppointmentsForTherapists(therapistIds).flatMap {
            existing =>
              val busy = existing
                .filter(a =>
                  !ignoring.contains(a.appointmentId) &&
                    overlaps(a.timeRange, timeRange)
                )
                .flatMap(_.therapistIds)
                .filter(therapistIds.contains)
                .distinct
              IO.raiseWhen(busy.nonEmpty)(TherapistBusy(busy))
          }

      // Fails with PatientBusy, carrying the patients who are busy
      private def failIfOverlappingForPatients(
          patientIds: List[PatientId],
          timeRange: TimeRange,
          ignoring: Option[AppointmentId]
      ): IO[Unit] =
        if !isValidRange(timeRange) then IO.unit
        else
          viewer.listAppointmentsForPatients(patientIds).flatMap { existing =>
            val busy = existing
              .filter(a =>
                !ignoring.contains(a.appointmentId) &&
                  overlaps(a.timeRange, timeRange)
              )
              .flatMap(_.patientIds)
              .filter(patientIds.contains)
              .distinct
            IO.raiseWhen(busy.nonEmpty)(PatientBusy(busy))
          }
