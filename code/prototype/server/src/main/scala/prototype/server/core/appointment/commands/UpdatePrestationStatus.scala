package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.StatusTransitions
import prototype.api.PrestationStatus
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*

import persistent4s.EventSourcedCommandHandler

final case class UpdatePrestationStatus(
    id: UUID,
    prestationId: UUID,
    newStatus: PrestationStatus
)

final case class UpdatePrestationState(
    appointmentExists: Boolean,
    prestationExists: Boolean,
    oldStatus: PrestationStatus
)

object UpdatePrestationStatus:

  // A prestation with no status event yet is still at its first status
  private def isAllowed(
      current: PrestationStatus,
      next: PrestationStatus
  ): Boolean =
    StatusTransitions.verifyStatusChange(
      current,
      next
    )

  object Handler
      extends EventSourcedCommandHandler[
        UpdatePrestationStatus,
        UpdatePrestationState,
        PrototypeEvent,
        AppointmentError
      ]:

    override protected val behavior = handler(
      initial = UpdatePrestationState(
        appointmentExists = false,
        prestationExists = false,
        oldStatus = PrestationStatus.TO_BILL
      )
    ):
      scope(AppointmentScopes.Appointment)(_.id)
      scope(AppointmentScopes.Prestation)(_.prestationId)

      on[AppointmentCreated].allEvents.evolve(_.copy(appointmentExists = true))
      on[AppointmentDeleted].allEvents.evolve(_.copy(appointmentExists = false))

      // The appointment's stream also holds its other prestations: only the
      // events of the prestation of the command count
      on[PrestationCreated].allEvents
        .matching(_.prestationId, _.prestationId)
        .evolve(
          _.copy(
            prestationExists = true,
            oldStatus = PrestationStatus.TO_BILL
          )
        )
      on[PrestationStatusUpdated].allEvents
        .matching(_.prestationId, _.prestationId)
        .evolve((state, event) => state.copy(oldStatus = event.status))
      on[PrestationDeleted].allEvents
        .matching(_.prestationId, _.prestationId)
        .evolve(_.copy(prestationExists = false))

      reject:
        case (state, command) if !state.appointmentExists =>
          APPOINTMENT_NOT_FOUND(command.id)
        case (state, command) if !state.prestationExists =>
          PRESTATION_NOT_FOUND(command.id, command.prestationId)
        case (state, command)
            if !isAllowed(state.oldStatus, command.newStatus) =>
          INVALID_STATUS_CHANGE(command.id, command.prestationId)

      emit: command =>
        PrestationStatusUpdated(
          command.id,
          command.prestationId,
          command.newStatus
        )
