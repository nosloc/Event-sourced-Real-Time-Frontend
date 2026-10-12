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
      eventStore: EventStore
  ): IO[AppointmentServiceImpl] =
    for
      view <- Ref.of[IO, Map[String, Appointment]](Map.empty)
      viewer = AppointmentViewer(view)
      decider = AppointmentDecider()
    yield new AppointmentServiceImpl:

      val handler = new CommandHandler(decider, viewer, eventStore)

      // TODO: check that the therapist exists (the old service did)
      override def listTherapistAppointments(
          therapistId: TherapistId
      ): IO[ListTherapistAppointmentsOutput] =
        viewer
          .listAppointments(therapistId.value)
          .map(ListTherapistAppointmentsOutput.apply)

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
        handler.handle(id.toString, command).flatMap {
          case Right(appt) => IO.pure(CreateAppointmentOutput(appt.get))
          case Left(INVALID_TIME_RANGE) =>
            IO.raiseError(InvalidTimeRange())
          case Left(error) => IO.raiseError(UserFacingError(error))
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
