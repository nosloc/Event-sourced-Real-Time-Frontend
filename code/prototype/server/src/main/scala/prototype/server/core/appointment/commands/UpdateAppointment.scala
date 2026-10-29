package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.*
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.patient.domain.{
  PatientCreated,
  PatientScopes
}
import prototype.server.core.therapist.domain.{
  TherapistCreated,
  TherapistScopes
}

import persistent4s.{EventSourcedCommandHandler, Tag}

/** Replaces the type, the participants and the time of an appointment: the
  * command holds the new values, so its scopes name everyone who will be in the
  * appointment. What changed (who joins, who leaves, the new type or time) is
  * worked out from what the appointment had, and only that is saved.
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

      // Someone added by an update only has this event in their stream: it tells them about the whole appointment
      on[TherapistAddedToAppointment].allEvents.evolve((state, event) =>
        state.afterTherapistAdded(event)
      )
      on[PatientAddedToAppointment].allEvents.evolve((state, event) =>
        state.afterPatientAdded(event)
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
          val addedTherapists =
            (command.therapistIds.toSet -- old.therapistIds).toList.sorted
          val removedTherapists =
            (old.therapistIds -- command.therapistIds).toList.sorted
          val addedPatients =
            (command.patientIds.toSet -- old.patientIds).toList
              .sortBy(_.toString)
          val removedPatients =
            (old.patientIds -- command.patientIds).toList.sortBy(_.toString)

          // The people who had the appointment: those who stay see the changes,
          // those who leave see they are free. The people who join are left
          // out, their stream has nothing to apply a change to
          val patchTags =
            Set(AppointmentScopes.Appointment(command.id).toTag) ++
              old.therapistIds.map(TherapistScopes.Therapist(_).toTag) ++
              old.patientIds.map(PatientScopes.Patient(_).toTag)

          val patch = AppointmentUpdated(
            command.id,
            therapistsAdded = addedTherapists,
            therapistsRemoved = removedTherapists,
            patientsAdded = addedPatients,
            patientsRemoved = removedPatients,
            apptType = Option.when(command.apptType != old.apptType)(
              command.apptType
            ),
            startTime = Option.when(start != old.start)(start),
            endTime = Option.when(end != old.end)(end)
          )

          // Each person who joins gets the whole appointment, as it is after
          // this update, in their own stream only
          val joined: List[(Set[Tag], PrototypeEvent)] =
            addedTherapists.map(therapist =>
              Set(TherapistScopes.Therapist(therapist).toTag) ->
                TherapistAddedToAppointment(
                  command.id,
                  therapist,
                  command.therapistIds.distinct,
                  command.patientIds.distinct,
                  command.apptType,
                  start,
                  end
                )
            ) ++ addedPatients.map(patient =>
              Set(PatientScopes.Patient(patient).toTag) ->
                PatientAddedToAppointment(
                  command.id,
                  patient,
                  command.therapistIds.distinct,
                  command.patientIds.distinct,
                  command.apptType,
                  start,
                  end
                )
            )
          (patchTags -> patch) :: joined
