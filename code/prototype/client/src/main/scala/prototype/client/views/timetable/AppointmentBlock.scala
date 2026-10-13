package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.api.*

object AppointmentBlock:
  def apply(
      appointment: Appointment,
      selectedAppointmentVar: Var[Option[Appointment]]
  ): HtmlElement =

    val Nbsp = "\u00a0"

    val start = appointment.timeRange.start.toDate
    val end = appointment.timeRange.end.toDate
    val decimalStart =
      start.decimalHours
    val decimalDuration =
      (end.getTime() - start.getTime()).toDouble / (1000.0 * 60.0 * 60.0)
    li(
      cls := "appt",
      cls := "appt--clickable",
      cls("appt--short") <-- Signal.fromValue(decimalDuration < 0.5),
      cls("appt--selected") <-- selectedAppointmentVar.signal.map {
        case Some(selected) =>
          selected.appointmentId == appointment.appointmentId
        case None => false
      },
      styleAttr := f"--s: $decimalStart%.2f; --d: $decimalDuration%.2f;",
      span(
        cls := "appt__time",
        s"${start.getTimeString} to${Nbsp}${end.getTimeString}"
      ),
      onClick --> { _ =>
        selectedAppointmentVar.set(Some(appointment))
      }
    )
  end apply
