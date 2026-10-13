package prototype.client.views.timetable

import prototype.api.*
import scala.scalajs.js.Date
import smithy4s.time.Timestamp

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

  final case class AppointmentFormState(
      date: String,
      start: String,
      end: String,
      showErrors: Boolean = false
  ):
    private def parse(d: String, t: String): Option[Date] =
      val parsed = new Date(s"${d}T$t")
      if parsed.getTime().isNaN() then None else Some(parsed)

    // A time string is checked on its own, against a fixed date, so that an
    // empty date does not also flag the times
    private def validTime(t: String): Boolean =
      parse("2000-01-01", t).isDefined

    def dateError: Option[String] =
      Option.when(parse(date, "00:00").isEmpty)("Pick a date")

    def startError: Option[String] =
      Option.when(!validTime(start))("Pick a start time")

    def endError: Option[String] =
      if !validTime(end) then Some("Pick an end time")
      else if parse(date, start).isDefined && toTimeRange.isEmpty then
        Some("End must be after start")
      else None

    private def toTimestamp(d: Date): Timestamp =
      Timestamp.fromEpochMilli(d.getTime().toLong)

    val toTimeRange: Option[TimeRange] =
      for
        startDate <- parse(date, start)
        endDate <- parse(date, end)
        if endDate.getTime() > startDate.getTime()
      yield TimeRange(toTimestamp(startDate), toTimestamp(endDate))

    def displayError(
        error: AppointmentFormState => Option[String]
    ): Option[String] =
      error(this).filter(_ => showErrors)
