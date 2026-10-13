package prototype.client.views.timetable

import prototype.api.*

object ErrorMessages:
  def errorToMessage(error: Throwable): String =
    error match
      case OverlappingAppointment() =>
        "The appointment overlaps with an existing appointment."
      case InvalidTimeRange()    => "The appointment time range is invalid."
      case AppointmentNotFound() => "The appointment was not found."
      case _ => s"An unexpected error occurred: ${error.getMessage}"
