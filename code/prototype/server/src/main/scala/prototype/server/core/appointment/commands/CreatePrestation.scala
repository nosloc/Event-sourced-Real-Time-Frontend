package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*

import persistent4s.EventSourcedCommandHandler

final case class CreatePrestation(
    id: UUID,
    prestationId: UUID,
    label: String,
    unitPrice: Float,
    quantity: Int
)

final case class CreatePrestationState(
    appointmentExists: Boolean,
    prestationExists: Boolean
)

object CreatePrestation:

  // The rule of the old service: a label, a price that is a real non-negative
  // number, and at least one unit
  private def isValid(command: CreatePrestation): Boolean =
    !command.label.isBlank &&
      command.unitPrice >= 0 &&
      !command.unitPrice.isNaN &&
      !command.unitPrice.isInfinite &&
      command.quantity > 0

  object Handler
      extends EventSourcedCommandHandler[
        CreatePrestation,
        CreatePrestationState,
        PrototypeEvent,
        AppointmentError
      ]:

    override protected val behavior =
      handler(initial = CreatePrestationState(false, false)):
        scope(AppointmentScopes.Appointment)(_.id)
        scope(AppointmentScopes.Prestation)(_.prestationId)

        on[AppointmentCreated].allEvents.evolve(_.copy(appointmentExists = true))
        on[AppointmentDeleted].allEvents.evolve(
          _.copy(appointmentExists = false)
        )

        on[PrestationCreated].allEvents
          .matching(_.prestationId, _.prestationId)
          .evolve(_.copy(prestationExists = true))

        on[PrestationDeleted].allEvents
          .matching(_.prestationId, _.prestationId)
          .evolve(_.copy(prestationExists = false))

        reject:
          case (s, command) if !s.appointmentExists =>
            APPOINTMENT_NOT_FOUND(command.id)
          case (_, command) if !isValid(command) =>
            INVALID_PRESTATION(command.id)
          case (s, command) if s.prestationExists =>
            PRESTATION_ALREADY_EXISTS(command.id, command.prestationId)

        emit: command =>
          PrestationCreated(
            command.id,
            command.prestationId,
            command.label,
            command.unitPrice,
            command.quantity
          )
