package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*

import persistent4s.EventSourcedCommandHandler

final case class DeletePrestation(
    id: UUID,
    prestationId: UUID
)

final case class DeletePrestationState(
    appointmentExists: Boolean,
    prestationExists: Boolean
)

object DeletePrestation:

  object Handler
      extends EventSourcedCommandHandler[
        DeletePrestation,
        DeletePrestationState,
        PrototypeEvent,
        AppointmentError
      ]:

    override protected val behavior =
      handler(initial = DeletePrestationState(false, false)):
        scope(AppointmentScopes.Appointment)(_.id)
        scope(AppointmentScopes.Prestation)(_.prestationId)

        on[AppointmentCreated].allEvents.evolve(
          _.copy(appointmentExists = true)
        )
        on[AppointmentDeleted].allEvents.evolve(
          _.copy(appointmentExists = false)
        )

        // The appointment's stream also holds its other prestations: only the
        // events of the prestation of the command count
        on[PrestationCreated].allEvents
          .matching(_.prestationId, _.prestationId)
          .evolve(_.copy(prestationExists = true))
        on[PrestationDeleted].allEvents
          .matching(_.prestationId, _.prestationId)
          .evolve(_.copy(prestationExists = false))

        // A prestation can be removed whatever its status, like in the old
        // service
        reject:
          case (state, command) if !state.appointmentExists =>
            APPOINTMENT_NOT_FOUND(command.id)
          case (state, command) if !state.prestationExists =>
            PRESTATION_NOT_FOUND(command.id, command.prestationId)

        emit: command =>
          PrestationDeleted(command.id, command.prestationId)
