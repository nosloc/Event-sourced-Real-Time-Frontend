package prototype.server.core.appointment

import prototype.server.eventStore.*
import prototype.server.core.appointment.appointmentModel.*

class AppointmentDecider
    extends Decider[
      AppointmentState,
      AppointmentCommand,
      AppointmentEvent,
      AppointmentError
    ]:

  def initialState: AppointmentState = None

  def decide(
      state: AppointmentState,
      command: AppointmentCommand
  ): Either[AppointmentError, List[AppointmentEvent]] =
    command match
      case CreateAppointmentCommand(
            appointmentId,
            therapistIds,
            patientIds,
            apptType,
            timeRange
          ) =>
        if therapistIds.isEmpty then Left(NO_THERAPIST)
        else if timeRange.end.toInstant.isAfter(timeRange.start.toInstant) then
          state match
            case None =>
              Right(
                List(
                  AppointmentCreated(
                    appointmentId,
                    therapistIds,
                    patientIds,
                    apptType,
                    timeRange
                  )
                )
              )
            case Some(_) =>
              Left(ALREADY_EXISTS)
        else Left(INVALID_TIME_RANGE)

      case UpdateAppointmentCommand(appointmentId, apptType, timeRange) =>
        if timeRange.end.toInstant.isAfter(timeRange.start.toInstant) then
          state match
            case Some(_) =>
              Right(
                List(
                  AppointmentUpdated(
                    appointmentId,
                    apptType,
                    timeRange
                  )
                )
              )
            case None =>
              Left(NOT_FOUND)
        else Left(INVALID_TIME_RANGE)

      case DeleteAppointmentCommand(appointmentId) =>
        state match
          case Some(_) =>
            Right(
              List(
                AppointmentDeleted(appointmentId)
              )
            )
          case None =>
            Left(NOT_FOUND)
  end decide

  def evolve(
      state: AppointmentState,
      event: AppointmentEvent
  ) = appointmentModel.evolve(state, event)
