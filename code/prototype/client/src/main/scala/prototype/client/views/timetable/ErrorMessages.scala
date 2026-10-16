package prototype.client.views.timetable

import prototype.api.*

/** An error found by the client itself before anything is sent (for example a
  * quantity that is not a number); its text is shown as it is.
  */
final case class InvalidInput(message: String) extends Exception(message)

object ErrorMessages:
  def errorToMessage(error: Throwable): String =
    error match
      case TherapistBusy(ids) =>
        s"Already booked at that time: ${ids.map(_.value).mkString(", ")}."
      case PatientBusy(_) =>
        "A patient already has an appointment at that time."
      case InvalidParticipants() =>
        "An appointment needs at least one therapist."
      case InvalidTimeRange()      => "The appointment time range is invalid."
      case AppointmentNotFound()   => "The appointment was not found."
      case TherapistsNotFound(ids) =>
        s"Therapist not found: ${ids.map(_.value).mkString(",")}."
      case PatientsNotFound(ids) =>
        s"Patient not found: ${ids.map(_.value).mkString(",")}."
      case InvalidPrestation() =>
        "A prestation needs a label, a quantity of at least 1 and a price of 0 or more."
      case InvalidStatusChange() => "This status change is not allowed."
      case PrestationNotFound()  => "The prestation was not found."
      case CompanyAlreadyExists() =>
        "A company with this name already exists."
      case TherapistAlreadyExists() =>
        "This trigram is already used by another therapist."
      case PatientAlreadyExists() =>
        "A patient with the same name and date of birth already exists."
      case CompanyNotFound()     => "The company was not found."
      case InvalidInput(message) => message

      case _ => s"An unexpected error occurred: ${error.getMessage}"
