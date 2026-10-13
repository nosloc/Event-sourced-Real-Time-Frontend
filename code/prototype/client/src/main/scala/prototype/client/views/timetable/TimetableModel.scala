package prototype.client.views.Timetable

import prototype.api.*
import scala.scalajs.js.Date

object TimetableModel:
  case class AppointmentWithDate(appointment: Appointment, date: Date)
  case class WeekRange(monday: Date, sunday: Date)

  case class DayColumn(
      stringKey: String,
      date: Date,
      orderedAppointments: List[AppointmentWithDate]
  )

  type WeekAppointments = List[DayColumn]

  val daysOfTheWeek: List[String] =
    List(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )

  val daysOfTheWeekShort: List[String] =
    List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
