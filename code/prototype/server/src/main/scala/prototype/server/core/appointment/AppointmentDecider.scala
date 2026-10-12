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
      case CreateAppointmentCommand(appointmentId, therapistId, timeRange) =>
        if timeRange.end.toInstant.isAfter(timeRange.start.toInstant) then
          state match
            case None =>
              Right(
                List(
                  AppointmentCreated(
                    appointmentId,
                    therapistId,
                    timeRange
                  )
                )
              )
            case Some(_) =>
              Left(AlreadyExists(appointmentId))
        else Left(InvalidRange(timeRange))

      case UpdateAppointmentCommand(appointmentId, timeRange, therapistId) =>
        if timeRange.end.toInstant.isAfter(timeRange.start.toInstant) then
          state match
            case Some(_) =>
              Right(
                List(
                  AppointmentUpdated(
                    appointmentId,
                    timeRange,
                    therapistId
                  )
                )
              )
            case None =>
              Left(NotFound(appointmentId))
        else Left(InvalidRange(timeRange))

      case DeleteAppointmentCommand(appointmentId) =>
        state match
          case Some(_) =>
            Right(
              List(
                AppointmentDeleted(appointmentId)
              )
            )
          case None =>
            Left(NotFound(appointmentId))
  end decide

  def evolve(
      state: AppointmentState,
      event: AppointmentEvent
  ) = appointmentModel.evolve(state, event)
