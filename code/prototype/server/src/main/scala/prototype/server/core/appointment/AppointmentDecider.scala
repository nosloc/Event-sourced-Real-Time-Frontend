package prototype.server.core.appointment

import prototype.server.eventStore.*
import prototype.server.core.appointment.appointmentModel.*
import prototype.api.*
import prototype.StatusTransitions.verifyStatusChange

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

      case UpdateAppointmentCommand(
            appointmentId,
            apptType,
            timeRange,
            therapistIds,
            patientIds
          ) =>
        if therapistIds.isEmpty then Left(NO_THERAPIST)
        else if timeRange.end.toInstant.isAfter(timeRange.start.toInstant) then
          state match
            case Some(_) =>
              Right(
                List(
                  AppointmentUpdated(
                    appointmentId,
                    apptType,
                    timeRange,
                    therapistIds,
                    patientIds
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
      case CreatePrestationCommand(
            appointmentId,
            prestationId,
            label,
            unitPrice,
            quantity
          ) =>
        state match
          case Some(_) =>
            if label.isBlank || unitPrice < 0 || unitPrice.isNaN || unitPrice.isInfinite || quantity <= 0
            then Left(INVALID_PRESTATION)
            else
              Right(
                List(
                  PrestationCreated(
                    appointmentId,
                    prestationId,
                    label,
                    unitPrice,
                    quantity
                  )
                )
              )
          case None =>
            Left(NOT_FOUND)
      case UpdatePrestationStatusCommand(
            appointmentId,
            prestationId,
            status
          ) =>
        state match
          case Some(appt) =>
            val prestationOpt =
              appt.prestations.find(_.prestationId == prestationId)
            prestationOpt match
              case Some(prestation) =>
                if verifyStatusChange(prestation.status, status) then

                  Right(
                    List(
                      PrestationStatusUpdated(
                        appointmentId,
                        prestationId,
                        status
                      )
                    )
                  )
                else Left(INVALID_STATUS_CHANGE)
              case None => Left(PRESTATION_NOT_FOUND)
          case None =>
            Left(NOT_FOUND)
      case DeletePrestationCommand(appointmentId, prestationId) =>
        state match
          case Some(appt) =>
            if appt.prestations.exists(_.prestationId == prestationId) then
              Right(
                List(
                  PrestationDeleted(
                    appointmentId,
                    prestationId
                  )
                )
              )
            else Left(PRESTATION_NOT_FOUND)
          case None =>
            Left(NOT_FOUND)

  end decide

  def evolve(
      state: AppointmentState,
      event: AppointmentEvent
  ) = appointmentModel.evolve(state, event)
  end evolve
