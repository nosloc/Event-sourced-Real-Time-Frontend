package prototype.server.core.appointment

import prototype.api.*

object appointmentModel:
  sealed trait AppointmentCommand
  case class CreateAppointment(
      appointmentId: AppointmentId,
      therapistId: TherapistId,
      timeRange: TimeRange
  ) extends AppointmentCommand
  case class UpdateAppointment(
      appointmentId: AppointmentId,
      timeRange: TimeRange,
      therapistId: TherapistId
  ) extends AppointmentCommand
  case class DeleteAppointment(appointmentId: AppointmentId)
      extends AppointmentCommand

  sealed trait AppointmentEvent
  case class AppointmentCreated(
      appointmentId: AppointmentId,
      therapistId: TherapistId,
      timeRange: TimeRange
  ) extends AppointmentEvent
  case class AppointmentUpdated(
      appointmentId: AppointmentId,
      timeRange: TimeRange,
      therapistId: TherapistId
  ) extends AppointmentEvent
  case class AppointmentDeleted(appointmentId: AppointmentId)
      extends AppointmentEvent

  sealed trait AppointmentError
  case class InvalidRange(timeRange: TimeRange) extends AppointmentError
  case class AlreadyExists(appointmentId: AppointmentId)
      extends AppointmentError
  case class NotFound(appointmentId: AppointmentId) extends AppointmentError

  type AppointmentState = Option[Appointment]

  // We put the evolve function here since it is the same for the decider and the viewer. Otherwise, we could have put it in the decider and the viewer separately.
  def evolve(
      state: AppointmentState,
      event: AppointmentEvent
  ): AppointmentState =
    event match
      case AppointmentCreated(appointmentId, therapistId, timeRange) =>
        Some(Appointment(appointmentId, therapistId, timeRange))
      case AppointmentUpdated(appointmentId, timeRange, therapistId) =>
        state.map(_.copy(timeRange = timeRange, therapistId = therapistId))
      case AppointmentDeleted(_) => None
