package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.api.*

object AppointmentBlock:
  def apply(
      appointmentS: Signal[Appointment],
      selectedAppointmentVar: Var[Option[Appointment]]
  ): HtmlElement =

    val Nbsp = "\u00a0"

    val startS = appointmentS.map(_.timeRange.start.toDate)
    val endS = appointmentS.map(_.timeRange.end.toDate)
    val decimalStartS =
      startS.map(_.decimalHours)
    val decimalDurationS =
      startS.combineWith(endS).map { case (start, end) =>
        (end.getTime() - start.getTime()).toDouble / (1000.0 * 60.0 * 60.0)
      }
    li(
      cls := "appt",
      cls := "appt--clickable",
      cls("appt--short") <-- decimalDurationS.map(_ < 0.5),
      cls("appt--selected") <-- selectedAppointmentVar.signal
        .combineWith(
          appointmentS
        )
        .map {
          case (Some(selected), appointment) =>
            selected.appointmentId == appointment.appointmentId
          case (None, _) => false
        },
      styleAttr <-- decimalStartS
        .combineWith(decimalDurationS)
        .map((start, duration) => f"--s: $start%.2f; --d: $duration%.2f;"),
      span(
        cls := "appt__time",
        text <-- startS
          .combineWith(endS)
          .map((start, end) =>
            s"${start.getTimeString} to${Nbsp}${end.getTimeString}"
          )
      ),
      onClick.compose(_.sample(appointmentS))
        --> { appointment =>
          selectedAppointmentVar.set(Some(appointment))
        }
    )
  end apply
