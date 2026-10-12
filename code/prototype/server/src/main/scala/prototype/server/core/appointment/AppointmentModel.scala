package prototype.server.core.appointment

import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.ErrorCode

object appointmentModel:
  sealed trait AppointmentCommand extends Command
  case class CreateAppointmentCommand(
      appointmentId: AppointmentId,
      therapistId: TherapistId,
      timeRange: TimeRange
  ) extends AppointmentCommand
  case class UpdateAppointmentCommand(
      appointmentId: AppointmentId,
      timeRange: TimeRange,
      therapistId: TherapistId
  ) extends AppointmentCommand
  case class DeleteAppointmentCommand(appointmentId: AppointmentId)
      extends AppointmentCommand

  sealed trait AppointmentEvent extends Event
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

  sealed trait AppointmentError extends ErrorCode
  case object INVALID_TIME_RANGE extends AppointmentError
  case object ALREADY_EXISTS extends AppointmentError
  case object NOT_FOUND extends AppointmentError

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
