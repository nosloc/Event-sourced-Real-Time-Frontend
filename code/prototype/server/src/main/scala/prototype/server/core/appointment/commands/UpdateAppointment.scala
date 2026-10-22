package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.api.*
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.patient.domain.{PatientCreated, PatientScopes}
import prototype.server.core.therapist.domain.{
  TherapistCreated,
  TherapistScopes
}

import persistent4s.EventSourcedCommandHandler

/** Replaces the type, the participants and the time of an appointment: the
  * values are the new ones, not a list of changes. Who left is worked out from
  * what the appointment had.
  */
final case class UpdateAppointment(
    id: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    timeRange: TimeRange
)

object UpdateAppointment:

  private def span(command: UpdateAppointment): (Long, Long) =
    (
      command.timeRange.start.toInstant.toEpochMilli,
      command.timeRange.end.toInstant.toEpochMilli
    )

  // The appointment itself is left out: it does not clash with its own old time
  private def busyTherapists(
      state: AppointmentsState,
      command: UpdateAppointment
  ): Set[String] =
    val (start, end) = span(command)
    state.overlappingTherapists(
      command.therapistIds,
      start,
      end,
      ignoring = Some(command.id)
    )

  private def busyPatients(
      state: AppointmentsState,
      command: UpdateAppointment
  ): Set[UUID] =
    val (start, end) = span(command)
    state.overlappingPatients(
      command.patientIds,
      start,
      end,
      ignoring = Some(command.id)
    )

  object Handler
      extends EventSourcedCommandHandler[
        UpdateAppointment,
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

      // No need to scope the removed participants
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

      // The same rules as the creation, once the appointment is known to exist
      reject:
        case (state, command) if !state.exists(command.id) =>
          APPOINTMENT_NOT_FOUND(command.id)
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

      emitManyTagged: (state, command) =>
        val (start, end) = span(command)
        val old = state.appointments(command.id)
        val unchanged =
          old == AppointmentSlot(
            command.therapistIds.toSet,
            command.patientIds.toSet,
            command.apptType,
            start,
            end
          )
        if unchanged then Nil
        else
          // Who left comes from the state: their streams must see the update to
          // know they are free, and the event itself only lists who stays
          val removedTherapists =
            (old.therapistIds -- command.therapistIds).toList.sorted
          val removedPatients =
            (old.patientIds -- command.patientIds).toList.sortBy(_.toString)
          // The removed ones must see they are out, and the added ones must
          // learn about the appointment
          val tags =
            Set(AppointmentScopes.Appointment(command.id).toTag) ++
              (command.therapistIds ++ removedTherapists)
                .map(TherapistScopes.Therapist(_).toTag) ++
              (command.patientIds ++ removedPatients)
                .map(PatientScopes.Patient(_).toTag)
          List(
            tags -> AppointmentUpdated(
              command.id,
              command.therapistIds,
              command.patientIds,
              command.apptType,
              start,
              end
            )
          )
