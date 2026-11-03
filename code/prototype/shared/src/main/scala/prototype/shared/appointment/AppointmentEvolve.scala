package prototype.shared.appointment

import java.util.UUID

import prototype.api.*
import prototype.shared.appointment.AppointmentInconsistency.{
  AlreadyExists,
  NotFound
}

/** The appointment fold, shared by the server projection and the browser.
  * `Left` when the event does not fit the state (see
  * `AppointmentInconsistency`); each side decides what to do with it.
  */
object AppointmentEvolve:
  def evolve(
      appointment: Option[AppointmentState],
      event: AppointmentEvent
  ): Either[AppointmentInconsistency, Option[AppointmentState]] =
    event match

      case created: AppointmentCreated =>
        if appointment.isDefined then Left(AlreadyExists(created))
        else
          Right(
            Some(
              newAppointment(
                created.id,
                created.therapistIds,
                created.patientIds,
                created.apptType,
                created.startTime,
                created.endTime
              )
            )
          )

      case _: AppointmentDeleted =>
        Right(None)

      // The update carries only what changed
      case updated: AppointmentUpdated =>
        existing(appointment, updated) { appointment =>
          val newTherapists = appointment.therapistIds
            .filterNot(therapist =>
              updated.therapistsRemoved.contains(therapist)
            ) ++
            updated.therapistsAdded
          val newPatients = appointment.patientIds
            .filterNot(patient => updated.patientsRemoved.contains(patient)) ++
            updated.patientsAdded

          val newTimeRange = (
            updated.startTime
              .getOrElse(
                appointment.startTime
              ),
            updated.endTime
              .getOrElse(
                appointment.endTime
              )
          )
          val newApptType = updated.apptType.getOrElse(appointment.apptType)
          appointment.copy(
            therapistIds = newTherapists.distinct,
            patientIds = newPatients.distinct,
            apptType = newApptType,
            startTime = newTimeRange._1,
            endTime = newTimeRange._2
          )
        }

      // A new prestation starts at its first status
      case created: PrestationCreated =>
        existing(appointment, created)(appointment =>
          appointment.copy(prestations =
            appointment.prestations :+ PrestationRow(
              prestationId = created.prestationId,
              label = created.label,
              quantity = created.quantity,
              unitPrice = created.unitPrice,
              status = PrestationStatus.TO_BILL
            )
          )
        )

      case updated: PrestationStatusUpdated =>
        existing(appointment, updated)(appointment =>
          appointment.copy(prestations =
            appointment.prestations.map(prestation =>
              if prestation.prestationId == updated.prestationId then
                prestation.copy(status = updated.status)
              else prestation
            )
          )
        )

      case deleted: PrestationDeleted =>
        existing(appointment, deleted)(appointment =>
          appointment.copy(prestations =
            appointment.prestations
              .filterNot(_.prestationId == deleted.prestationId)
          )
        )

      // Whoever is added gets the whole appointment. An appointment already
      // known gets the update itself, and keeps its prestations
      case added: TherapistAddedToAppointment =>
        Right(
          appointment.orElse(
            Some(
              newAppointment(
                added.id,
                added.appointmentTherapists,
                added.appointmentPatients,
                added.appointmentType,
                added.appointmentStartTime,
                added.appointmentEndTime
              )
            )
          )
        )

      case _: PatientAddedToAppointment => Right(appointment)

  // A change needs the appointment it is about
  private def existing(
      appointment: Option[AppointmentState],
      event: AppointmentEvent
  )(
      change: AppointmentState => AppointmentState
  ): Either[AppointmentInconsistency, Option[AppointmentState]] =
    appointment.map(change).toRight(NotFound(event)).map(Some(_))

  // None for an unknown appointment type
  private def newAppointment(
      id: UUID,
      therapistIds: List[String],
      patientIds: List[UUID],
      apptType: AppointmentType,
      startTime: Long,
      endTime: Long
  ): AppointmentState =
    AppointmentState(
      id,
      therapistIds,
      patientIds,
      apptType,
      startTime,
      endTime,
      Nil
    )
