package prototype.server.core.appointment

import prototype.api.*
import prototype.server.ErrorCode
import prototype.server.eventStore.*

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
      apptType: AppointmentType,
      timeRange: TimeRange,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId]
  ) extends AppointmentCommand
  case class DeleteAppointmentCommand(appointmentId: AppointmentId)
      extends AppointmentCommand
  case class CreatePrestationCommand(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      label: String,
      unitPrice: Float,
      quantity: Int
  ) extends AppointmentCommand
  case class UpdatePrestationStatusCommand(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      status: PrestationStatus
  ) extends AppointmentCommand
  case class DeletePrestationCommand(
      appointmentId: AppointmentId,
      prestationId: PrestationId
  ) extends AppointmentCommand

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
      apptType: AppointmentType,
      timeRange: TimeRange,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId]
  ) extends AppointmentEvent
  case class AppointmentDeleted(appointmentId: AppointmentId)
      extends AppointmentEvent
  case class PrestationCreated(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      label: String,
      unitPrice: Float,
      quantity: Int
  ) extends AppointmentEvent
  case class PrestationStatusUpdated(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      status: PrestationStatus
  ) extends AppointmentEvent
  case class PrestationDeleted(
      appointmentId: AppointmentId,
      prestationId: PrestationId
  ) extends AppointmentEvent

  sealed trait AppointmentError extends ErrorCode
  case object INVALID_TIME_RANGE extends AppointmentError
  case object NO_THERAPIST extends AppointmentError
  case object ALREADY_EXISTS extends AppointmentError
  case object PRESTATION_NOT_FOUND extends AppointmentError
  case object INVALID_STATUS_CHANGE extends AppointmentError
  case object INVALID_PRESTATION extends AppointmentError
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
            timeRange,
            List.empty[Prestation]
          )
        )
      case AppointmentUpdated(
            appointmentId,
            apptType,
            timeRange,
            therapistIds,
            patientIds
          ) =>
        state.map(
          _.copy(
            timeRange = timeRange,
            appointmentType = apptType,
            therapistIds = therapistIds,
            patientIds = patientIds
          )
        )
      case AppointmentDeleted(_) => None
      case PrestationCreated(
            appointmentId,
            prestationId,
            label,
            unitPrice,
            quantity
          ) =>
        state.map(s =>
          s.copy(
            prestations = s.prestations :+ Prestation(
              s.appointmentId,
              prestationId,
              label,
              quantity,
              unitPrice,
              PrestationStatus.TO_BILL
            )
          )
        )
      case PrestationStatusUpdated(
            appointmentId,
            prestationId,
            status
          ) =>
        state.map(s =>
          s.copy(
            prestations = s.prestations.map { p =>
              if p.prestationId == prestationId then p.copy(status = status)
              else p
            }
          )
        )
      case PrestationDeleted(appointmentId, prestationId) =>
        state.map(s =>
          s.copy(
            prestations =
              s.prestations.filterNot(_.prestationId == prestationId)
          )
        )
