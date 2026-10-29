package prototype.client.appointments

import prototype.api.*
import smithy4s.time.Timestamp
import java.util.UUID

/** How one appointment changes with an event: the same rules as the server's
  * appointment projection, plus the "added to appointment" events, which the
  * projection ignores. Here they only build an appointment not known yet.
  * `None`: no such appointment, or it was deleted.
  */
object AppointmentEvolve:
  def evolve(
      appointment: Option[Appointment],
      event: AppointmentEvent
  ): Option[Appointment] =
    event match

      case created: AppointmentCreated if appointment.isEmpty =>
        newAppointment(
          created.id,
          created.therapistIds,
          created.patientIds,
          created.apptType,
          created.startTime,
          created.endTime
        )

      case _: AppointmentDeleted =>
        None

      // The update carries only what changed
      case updated: AppointmentUpdated =>
        appointment.map { appointment =>
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
        }

      // A new prestation starts at its first status
      case created: PrestationCreated =>
        appointment.map(appointment =>
          appointment.copy(prestations =
            appointment.prestations :+ Prestation(
              appointmentId = AppointmentId(created.id),
              prestationId = PrestationId(created.prestationId),
              label = created.label,
              quantity = created.quantity,
              unitPrice = created.unitPrice,
              status = PrestationStatus.TO_BILL
            )
          )
        )

      case updated: PrestationStatusUpdated =>
        appointment.map(appointment =>
          appointment.copy(prestations =
            appointment.prestations.map(prestation =>
              if prestation.prestationId.value == updated.prestationId then
                prestation.copy(status = updated.status)
              else prestation
            )
          )
        )

      case deleted: PrestationDeleted =>
        appointment.map(appointment =>
          appointment.copy(prestations =
            appointment.prestations
              .filterNot(_.prestationId.value == deleted.prestationId)
          )
        )

      // A creation for an appointment that exists already is ignored
      case _: AppointmentCreated => appointment

      // Whoever is added gets the whole appointment. An appointment already
      // known gets the update itself, and keeps its prestations
      case added: TherapistAddedToAppointment =>
        newAppointment(
          added.id,
          added.appointmentTherapists,
          added.appointmentPatients,
          added.appointmentType,
          added.appointmentStartTime,
          added.appointmentEndTime
        )

      case _: PatientAddedToAppointment => appointment

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
