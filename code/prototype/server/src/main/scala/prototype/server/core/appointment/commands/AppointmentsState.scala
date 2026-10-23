package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.server.core.appointment.domain.{
  AppointmentCreated,
  AppointmentDeleted,
  AppointmentUpdated
}
import prototype.server.core.patient.domain.PatientAddedToAppointment
import prototype.server.core.therapist.domain.TherapistAddedToAppointment

/** What an existing appointment looks like in the history read (times in epoch
  * milliseconds, as in the events).
  */
final case class AppointmentSlot(
    therapistIds: Set[String],
    patientIds: Set[UUID],
    apptType: String,
    start: Long,
    end: Long
)

/** What the appointment handlers know from the history they read: the
  * therapists and patients that exist, and the live appointments by id. Who is
  * busy is not stored: it is derived from the appointments when a command is
  * decided.
  */
final case class AppointmentsState(
    foundTherapists: Set[String],
    foundPatients: Set[UUID],
    appointments: Map[UUID, AppointmentSlot] = Map.empty
):

  def exists(id: UUID): Boolean = appointments.contains(id)

  // Return overlapping appointments, ignoring the one with the given id if any
  private def overlapping(
      start: Long,
      end: Long,
      ignoring: Option[UUID]
  ): Iterable[AppointmentSlot] =
    appointments.collect {
      case (id, slot)
          if !ignoring.contains(id) && slot.start < end && start < slot.end =>
        slot
    }

  // Return Therapists with overlapping appointments, ignoring the appointment with the given id if any
  def overlappingTherapists(
      therapistIds: List[String],
      start: Long,
      end: Long,
      ignoring: Option[UUID]
  ): Set[String] =
    overlapping(start, end, ignoring).flatMap(_.therapistIds).toSet &
      therapistIds.toSet

  // Return Patients with overlapping appointments, ignoring the appintment with the given id if any
  def overlappingPatients(
      patientIds: List[UUID],
      start: Long,
      end: Long,
      ignoring: Option[UUID]
  ): Set[UUID] =
    overlapping(start, end, ignoring).flatMap(_.patientIds).toSet &
      patientIds.toSet

  // Put the full appointment under its id, replacing what was there
  private def withSlot(
      id: UUID,
      therapistIds: List[String],
      patientIds: List[UUID],
      apptType: String,
      start: Long,
      end: Long
  ): AppointmentsState =
    copy(appointments =
      appointments.updated(
        id,
        AppointmentSlot(
          therapistIds.toSet,
          patientIds.toSet,
          apptType,
          start,
          end
        )
      )
    )

  // Update the state when an appointment is created
  def afterCreated(event: AppointmentCreated): AppointmentsState =
    withSlot(
      event.id,
      event.therapistIds,
      event.patientIds,
      event.apptType,
      event.startTime,
      event.endTime
    )

  // The event holds the full new values, so it does not need an earlier event
  // of the same appointment in the history read
  def afterUpdated(event: AppointmentUpdated): AppointmentsState =
    withSlot(
      event.id,
      event.therapistIds,
      event.patientIds,
      event.apptType,
      event.startTime,
      event.endTime
    )

  // Remove the appointment from the state when it is deleted
  def afterDeleted(event: AppointmentDeleted): AppointmentsState =
    copy(appointments = appointments - event.id)

  // Update the state when a therapist is added to an appointment
  def afterTherapistAdded(
      event: TherapistAddedToAppointment
  ): AppointmentsState =
    withSlot(
      event.appointmentId,
      event.appointmentTherapists,
      event.appointmentPatients,
      event.appointmentType,
      event.appointmentStartTime,
      event.appointmentEndTime
    )

  // Update the state when a patient is added to an appointment
  def afterPatientAdded(
      event: PatientAddedToAppointment
  ): AppointmentsState =
    withSlot(
      event.appointmentId,
      event.appointmentTherapists,
      event.appointmentPatients,
      event.appointmentType,
      event.appointmentStartTime,
      event.appointmentEndTime
    )
