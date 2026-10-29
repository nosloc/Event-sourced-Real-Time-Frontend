package prototype.client.appointments

import prototype.api.*
import smithy4s.time.Timestamp
import java.util.UUID

object AppointmentEvolve:
  def evolve(
      trigram: String,
      appointments: List[Appointment],
      event: AppointmentEvent
  ): List[Appointment] =
    event match

      // Add the appointment if the therapist is in it and it is not listed yet
      case created: AppointmentCreated
          if created.therapistIds.contains(trigram) &&
            !appointments.exists(_.appointmentId.value == created.id) =>
        newAppointment(
          created.id,
          created.therapistIds,
          created.patientIds,
          created.apptType,
          created.startTime,
          created.endTime
        ).fold(appointments)(appointment => appointments :+ appointment)

      // Remove the appointment from the list
      case AppointmentDeleted(id) =>
        appointments.filterNot(_.appointmentId.value == id)

      // Remove the appointment if the current therapist is removed from it
      case updated: AppointmentUpdated
          if updated.therapistsRemoved.contains(trigram) =>
        appointments.filterNot(_.appointmentId.value == updated.id)

      // The therapist joins: TherapistAddedToAppointment brings the whole
      // appointment, this update alone cannot build it
      case updated: AppointmentUpdated
          if updated.therapistsAdded.contains(trigram) =>
        appointments

      // Apply what changed to the appointment, if it is listed
      case updated: AppointmentUpdated =>
        appointments.map { appointment =>
          if (appointment.appointmentId.value == updated.id)
            val newTherapists = appointment.therapistIds
              .filterNot(therapist =>
                updated.therapistsRemoved.contains(therapist.value)
              ) ++
              updated.therapistsAdded.map(TherapistId.apply)
            val newPatients = appointment.patientIds
              .filterNot(patient =>
                updated.patientsRemoved.contains(patient.value)
              ) ++
              updated.patientsAdded.map(PatientId.apply)
            val newType = updated.apptType
              .flatMap(apptType =>
                AppointmentType.values.find(_.stringValue == apptType)
              )
              .getOrElse(appointment.appointmentType)
            val newTimeRange = TimeRange(
              updated.startTime
                .map(Timestamp.fromEpochMilli)
                .getOrElse(
                  appointment.timeRange.start
                ),
              updated.endTime
                .map(Timestamp.fromEpochMilli)
                .getOrElse(
                  appointment.timeRange.end
                )
            )
            appointment.copy(
              therapistIds = newTherapists.distinct,
              patientIds = newPatients.distinct,
              appointmentType = newType,
              timeRange = newTimeRange
            )
          else appointment
        }

      // Add the appointment if this therapist is the one added and it is not
      // listed yet
      case added: TherapistAddedToAppointment
          if added.id == trigram && !appointments.exists(
            _.appointmentId.value == added.appointmentId
          ) =>
        newAppointment(
          added.appointmentId,
          added.appointmentTherapists,
          added.appointmentPatients,
          added.appointmentType,
          added.appointmentStartTime,
          added.appointmentEndTime
        ).fold(appointments)(appointment => appointments :+ appointment)

      // Another therapist's appointment, or one already listed
      case _: AppointmentCreated          => appointments
      case _: TherapistAddedToAppointment => appointments

      // TODO: Handle the remaining events
      case _ => appointments

  // None for an unknown appointment type
  private def newAppointment(
      id: UUID,
      therapistIds: List[String],
      patientIds: List[UUID],
      apptType: String,
      startTime: Long,
      endTime: Long
  ): Option[Appointment] =
    AppointmentType.values
      .find(_.stringValue == apptType)
      .map(appointmentType =>
        Appointment(
          AppointmentId(id),
          therapistIds.map(TherapistId.apply),
          patientIds.map(PatientId.apply),
          appointmentType,
          // The events hold epoch milliseconds
          TimeRange(
            Timestamp.fromEpochMilli(startTime),
            Timestamp.fromEpochMilli(endTime)
          ),
          Nil
        )
      )
