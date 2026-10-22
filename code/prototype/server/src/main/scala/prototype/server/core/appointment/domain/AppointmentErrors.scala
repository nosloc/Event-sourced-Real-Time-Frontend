package prototype.server.core.appointment.domain

import java.util.UUID

import prototype.server.UserFacingFailure

/** Explains why an appointment command was refused.
  *
  * @param message
  *   explanation shown to the caller
  */
enum AppointmentError(val message: String)
    extends Exception(message),
      UserFacingFailure:

  /** No appointment uses the given id
    *
    * @param id
    *   appointment id
    */
  case APPOINTMENT_NOT_FOUND(id: UUID)
      extends AppointmentError(s"Appointment $id does not exist.")

  /** An appointment already uses the given id
    *
    * @param id
    *   appointment id
    */
  case APPOINTMENT_ALREADY_EXISTS(id: UUID)
      extends AppointmentError(s"Appointment $id already exists.")

  /** The appointment does not end after it starts
    *
    * @param id
    *   appointment id
    */
  case INVALID_TIME_RANGE(id: UUID)
      extends AppointmentError(s"Appointment $id must end after it starts.")

  /** The appointment has no therapist
    *
    * @param id
    *   appointment id
    */
  case NO_THERAPIST(id: UUID)
      extends AppointmentError(s"Appointment $id needs at least one therapist.")

  /** Some therapists of the appointment do not exist
    *
    * @param therapistIds
    *   the therapist ids (trigrams) that were not found
    */
  case THERAPISTS_NOT_FOUND(therapistIds: List[String])
      extends AppointmentError(
        s"These therapists do not exist: ${therapistIds.mkString(", ")}."
      )

  /** Some patients of the appointment do not exist
    *
    * @param patientIds
    *   the patient ids that were not found
    */
  case PATIENTS_NOT_FOUND(patientIds: List[UUID])
      extends AppointmentError(
        s"These patients do not exist: ${patientIds.mkString(", ")}."
      )

  /** Some therapists already have an appointment at that time
    *
    * @param therapistIds
    *   the therapist ids (trigrams) that are busy
    */
  case THERAPIST_BUSY(therapistIds: List[String])
      extends AppointmentError(
        s"These therapists are busy at that time: ${therapistIds.mkString(", ")}."
      )

  /** Some patients already have an appointment at that time
    *
    * @param patientIds
    *   the patient ids that are busy
    */
  case PATIENT_BUSY(patientIds: List[UUID])
      extends AppointmentError(
        s"These patients are busy at that time: ${patientIds.mkString(", ")}."
      )

  /** The appointment has no prestation with the given id
    *
    * @param id
    *   appointment id
    * @param prestationId
    *   prestation id
    */
  case PRESTATION_NOT_FOUND(id: UUID, prestationId: UUID)
      extends AppointmentError(
        s"Appointment $id has no prestation $prestationId."
      )

  /** The appointment already has a prestation with the given id
    *
    * @param id
    *   appointment id
    * @param prestationId
    *   prestation id
    */
  case PRESTATION_ALREADY_EXISTS(id: UUID, prestationId: UUID)
      extends AppointmentError(
        s"Appointment $id already has a prestation $prestationId."
      )

  /** The prestation is invalid: blank label, negative or non-finite price, or a
    * quantity of 0 or less
    *
    * @param id
    *   appointment id
    */
  case INVALID_PRESTATION(id: UUID)
      extends AppointmentError(s"A prestation of appointment $id is invalid.")

  /** The prestation cannot go from its current status to the requested one
    *
    * @param id
    *   appointment id
    * @param prestationId
    *   prestation id
    */
  case INVALID_STATUS_CHANGE(id: UUID, prestationId: UUID)
      extends AppointmentError(
        s"Prestation $prestationId of appointment $id cannot change to that status."
      )
