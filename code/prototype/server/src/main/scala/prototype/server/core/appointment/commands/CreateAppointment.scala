package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.patient.domain.{
  PatientAddedToAppointment,
  PatientCreated,
  PatientScopes
}
import prototype.server.core.therapist.domain.{
  TherapistAddedToAppointment,
  TherapistCreated,
  TherapistScopes
}

import persistent4s.EventSourcedCommandHandler

final case class CreateAppointment(
    id: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    timeRange: TimeRange
)

object CreateAppointment:

  // The two ends of the command's time range, in epoch milliseconds like the events
  private def span(command: CreateAppointment): (Long, Long) =
    (
      command.timeRange.start.toInstant.toEpochMilli,
      command.timeRange.end.toInstant.toEpochMilli
    )

  private def busyTherapists(
      state: AppointmentsState,
      command: CreateAppointment
  ): Set[String] =
    val (start, end) = span(command)
    state.overlappingTherapists(
      command.therapistIds,
      start,
      end,
      ignoring = None
    )

  private def busyPatients(
      state: AppointmentsState,
      command: CreateAppointment
  ): Set[UUID] =
    val (start, end) = span(command)
    state.overlappingPatients(command.patientIds, start, end, ignoring = None)

  object Handler
      extends EventSourcedCommandHandler[
        CreateAppointment,
        AppointmentsState,
        PrototypeEvent,
        AppointmentError
      ]:

    override protected val behavior = handler(
      initial = AppointmentsState(
        foundTherapists = Set.empty,
        foundPatients = Set.empty
      )
    ):
      scope(AppointmentScopes.Appointment)(_.id)

      scopeMany(TherapistScopes.Therapist)(_.therapistIds)
      scopeMany(PatientScopes.Patient)(_.patientIds)

      // The therapists and patients of the command exist if their own creation
      // event is in the history read
      on[TherapistCreated].allEvents.evolve((state, event) =>
        state.copy(foundTherapists = state.foundTherapists + event.id)
      )
      on[PatientCreated].allEvents.evolve((state, event) =>
        state.copy(foundPatients = state.foundPatients + event.id)
      )

      // The appointments of those therapists and patients, as they are now
      on[AppointmentCreated].allEvents.evolve((state, event) =>
        state.afterCreated(event)
      )
      on[AppointmentUpdated].allEvents.evolve((state, event) =>
        state.afterUpdated(event)
      )
      on[AppointmentDeleted].allEvents.evolve((state, event) =>
        state.afterDeleted(event)
      )

      // Someone added by an update only has this event in their stream: it tells them about the whole appointment
      on[TherapistAddedToAppointment].allEvents.evolve((state, event) =>
        state.afterTherapistAdded(event)
      )
      on[PatientAddedToAppointment].allEvents.evolve((state, event) =>
        state.afterPatientAdded(event)
      )

      reject:
        case (state, command) if state.exists(command.id) =>
          APPOINTMENT_ALREADY_EXISTS(command.id)
        case (_, command) if command.therapistIds.isEmpty =>
          NO_THERAPIST(command.id)
        case (_, command)
            if !command.timeRange.end.toInstant.isAfter(
              command.timeRange.start.toInstant
            ) =>
          INVALID_TIME_RANGE(command.id)
        case (state, command)
            if command.therapistIds
              .exists(id => !state.foundTherapists.contains(id)) =>
          THERAPISTS_NOT_FOUND(
            command.therapistIds.distinct
              .filterNot(state.foundTherapists.contains)
          )
        case (state, command) if busyTherapists(state, command).nonEmpty =>
          THERAPIST_BUSY(
            command.therapistIds.distinct
              .filter(busyTherapists(state, command).contains)
          )
        case (state, command)
            if command.patientIds
              .exists(id => !state.foundPatients.contains(id)) =>
          PATIENTS_NOT_FOUND(
            command.patientIds.distinct.filterNot(state.foundPatients.contains)
          )
        case (state, command) if busyPatients(state, command).nonEmpty =>
          PATIENT_BUSY(
            command.patientIds.distinct
              .filter(busyPatients(state, command).contains)
          )

      emit: command =>
        AppointmentCreated(
          command.id,
          command.therapistIds,
          command.patientIds,
          command.apptType,
          command.timeRange.start.toInstant.toEpochMilli,
          command.timeRange.end.toInstant.toEpochMilli
        )
