package prototype.server.core.appointment

import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.ErrorCode

object appointmentModel:
  sealed trait AppointmentCommand extends Command
  case class CreateAppointmentCommand(
      appointmentId: AppointmentId,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
      apptType: AppointmentType,
      timeRange: TimeRange
  ) extends AppointmentCommand
  case class UpdateAppointmentCommand(
      appointmentId: AppointmentId,
      timeRange: TimeRange
  ) extends AppointmentCommand
  case class DeleteAppointmentCommand(appointmentId: AppointmentId)
      extends AppointmentCommand

  sealed trait AppointmentEvent extends Event
  case class AppointmentCreated(
      appointmentId: AppointmentId,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
      apptType: AppointmentType,
      timeRange: TimeRange
  ) extends AppointmentEvent
  case class AppointmentUpdated(
      appointmentId: AppointmentId,
      timeRange: TimeRange
  ) extends AppointmentEvent
  case class AppointmentDeleted(appointmentId: AppointmentId)
      extends AppointmentEvent

  sealed trait AppointmentError extends ErrorCode
  case object INVALID_TIME_RANGE extends AppointmentError
  case object NO_THERAPIST extends AppointmentError
  case object ALREADY_EXISTS extends AppointmentError
  case object NOT_FOUND extends AppointmentError

  type AppointmentState = Option[Appointment]

  /** Two time ranges overlap when each starts before the other ends, so
    * back-to-back appointments (one ends exactly when the next starts) do not.
    */
  def overlaps(a: TimeRange, b: TimeRange): Boolean =
    a.start.toInstant.isBefore(b.end.toInstant) &&
      b.start.toInstant.isBefore(a.end.toInstant)

  def isValidRange(timeRange: TimeRange): Boolean =
    timeRange.end.toInstant.isAfter(timeRange.start.toInstant)

  // We put the evolve function here since it is the same for the decider and the viewer. Otherwise, we could have put it in the decider and the viewer separately.
  def evolve(
      state: AppointmentState,
      event: AppointmentEvent
  ): AppointmentState =
    event match
      case AppointmentCreated(
            appointmentId,
            therapistIds,
            patientIds,
            apptType,
            timeRange
          ) =>
        Some(
          Appointment(
            appointmentId,
            therapistIds,
            patientIds,
            apptType,
            timeRange
          )
        )
      case AppointmentUpdated(appointmentId, timeRange) =>
        state.map(_.copy(timeRange = timeRange))
      case AppointmentDeleted(_) => None
