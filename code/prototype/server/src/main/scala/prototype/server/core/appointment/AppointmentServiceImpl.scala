package prototype.server.core.appointment

import cats.effect.*
import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.core.appointment.appointmentModel.*
import java.util.UUID
import prototype.server.UserFacingError

trait AppointmentServiceImpl extends AppointmentService[IO]

object AppointmentServiceImpl:
  def apply(
      eventStore: EventStore,
      therapistService: TherapistService[IO]
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
            .listAppointments(therapistId.value)
            .map(ListTherapistAppointmentsOutput.apply)
        }

      override def createAppointment(
          therapistId: TherapistId,
          timeRange: TimeRange
      ): IO[CreateAppointmentOutput] =
        val id = UUID.randomUUID()
        val command = CreateAppointmentCommand(
          AppointmentId(id),
          therapistId,
          timeRange
        )
        // Fails with TherapistNotFound if the therapist does not exist, and
        // with OverlappingAppointment if the therapist is busy at that time
        therapistService.getTherapist(therapistId).flatMap { _ =>
          failIfOverlapping(therapistId, timeRange, ignoring = None) *>
            handler.handle(id.toString, command).flatMap {
              case Right(appt) => IO.pure(CreateAppointmentOutput(appt.get))
              case Left(INVALID_TIME_RANGE) =>
                IO.raiseError(InvalidTimeRange())
              case Left(error) => IO.raiseError(UserFacingError(error))
            }
        }

      override def deleteAppointment(
          appointmentId: AppointmentId
      ): IO[Unit] =
        val command = DeleteAppointmentCommand(appointmentId)
        handler.handle(appointmentId.value.toString, command).flatMap {
          case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
          case Left(error)     => IO.raiseError(UserFacingError(error))
          case Right(_)        => IO.unit
        }

      // The therapist of an appointment does not change: the update keeps it
      override def updateAppointment(
          appointmentId: AppointmentId,
          timeRange: TimeRange
      ): IO[UpdateAppointmentOutput] =
        viewer.getState(appointmentId.value.toString).flatMap {
          case None          => IO.raiseError(AppointmentNotFound())
          case Some(current) =>
            val command = UpdateAppointmentCommand(
              appointmentId,
              timeRange,
              current.therapistId
            )
            failIfOverlapping(
              current.therapistId,
              timeRange,
              ignoring = Some(appointmentId)
            ) *>
              handler.handle(appointmentId.value.toString, command).flatMap {
                case Right(appt) =>
                  IO.pure(UpdateAppointmentOutput(appt.get))
                case Left(INVALID_TIME_RANGE) =>
                  IO.raiseError(InvalidTimeRange())
                case Left(NOT_FOUND) => IO.raiseError(AppointmentNotFound())
                case Left(error)     => IO.raiseError(UserFacingError(error))
              }
        }

      private def failIfOverlapping(
          therapistId: TherapistId,
          timeRange: TimeRange,
          ignoring: Option[AppointmentId]
      ): IO[Unit] =
        if !isValidRange(timeRange) then IO.unit
        else
          viewer.listAppointments(therapistId.value).flatMap { existing =>
            val busy = existing.exists(a =>
              !ignoring.contains(a.appointmentId) &&
                overlaps(a.timeRange, timeRange)
            )
            IO.raiseWhen(busy)(OverlappingAppointment())
          }
