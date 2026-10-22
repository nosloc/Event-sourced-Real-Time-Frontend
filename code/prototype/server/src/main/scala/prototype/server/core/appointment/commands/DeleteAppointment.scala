package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.patient.domain.PatientScopes
import prototype.server.core.therapist.domain.TherapistScopes

import persistent4s.EventSourcedCommandHandler

final case class DeleteAppointment(
    id: UUID
)

object DeleteAppointment:

  object Handler
      extends EventSourcedCommandHandler[
        DeleteAppointment,
        AppointmentsState,
        PrototypeEvent,
        AppointmentError
      ]:

    // Only the appointment's own stream is read: it holds its creation and its
    // updates, so it knows who is in it now
    override protected val behavior = handler(
      initial = AppointmentsState(
        foundTherapists = Set.empty,
        foundPatients = Set.empty
      )
    ):
      scope(AppointmentScopes.Appointment)(_.id)

      on[AppointmentCreated].allEvents.evolve((state, event) =>
        state.afterCreated(event)
      )
      on[AppointmentUpdated].allEvents.evolve((state, event) =>
        state.afterUpdated(event)
      )
      on[AppointmentDeleted].allEvents.evolve((state, event) =>
        state.afterDeleted(event)
      )

      reject:
        case (state, command) if !state.exists(command.id) =>
          APPOINTMENT_NOT_FOUND(command.id)

      // Tagged with the current participants, so that their streams see that
      // the appointment is gone and they are free again
      emitTagged: (state, command) =>
        val slot = state.appointments(command.id)
        val tags =
          Set(AppointmentScopes.Appointment(command.id).toTag) ++
            slot.therapistIds.map(TherapistScopes.Therapist(_).toTag) ++
            slot.patientIds.map(PatientScopes.Patient(_).toTag)
        tags -> AppointmentDeleted(command.id)
