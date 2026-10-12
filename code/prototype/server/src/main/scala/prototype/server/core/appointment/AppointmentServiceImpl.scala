package prototype.server.core.appointment

import cats.effect.*
import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.core.appointment.appointmentModel.*
import java.util.UUID

trait AppointmentServiceImpl extends AppointmentService[IO]:
  def createAppointment(
      timeRange: TimeRange,
      therapistId: TherapistId
  ): IO[Unit]

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

      def createAppointment(
          timeRange: TimeRange,
          therapistId: TherapistId
      ): IO[Unit] =
        val id = UUID.randomUUID()
        val command = CreateAppointmentCommand(
          AppointmentId(id),
          therapistId,
          timeRange
        )
        handler.handle(id.toString, command).flatMap {
          case Left(error) => IO.raiseError(new Exception(error.toString))
          case Right(_)    => IO.unit
        }
