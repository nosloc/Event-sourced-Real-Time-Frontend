package prototype.client.views.timetable

import prototype.api.*

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

      case _ => s"An unexpected error occurred: ${error.getMessage}"
