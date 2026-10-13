package prototype.client.Views.Timetable

import com.raquo.laminar.api.L.*
import prototype.client.Views.Timetable.TimetableModel.*
import scala.scalajs.js.Date
import prototype.api.*

object WeekGrid:

  def apply(
      appointmentsS: Signal[WeekAppointments],
      errorS: Signal[Option[Throwable]],
      loadingS: Signal[Boolean],
      currentTimeS: Signal[Double],
      weekDaysS: Signal[List[Date]],
      selectedAppointmentVar: Var[Option[Appointment]],
      today: Date
  ): HtmlElement =

    val inWindowS: Signal[Boolean] = currentTimeS.map { current =>
      current >= windowStart && current < windowEnd
    }.distinct

    div(
      child.maybe <-- errorS.map {
        case Some(error) =>
          Some(
            div(
              cls := "banner banner--error",
              s"Error loading appointments: ${error.getMessage}"
            )
          )
        case None => None
      },
      div(
        cls := "skeleton skeleton--grid",
        hidden <-- loadingS.map(!_)
      ),
      div(
        cls := "timetable__scroll",
        hidden <-- loadingS,
        table(
          cls := "timetable__table",
          renderTableHeader(weekDaysS, today),
          renderTableBody(
            appointmentsS,
            currentTimeS,
            inWindowS,
            selectedAppointmentVar,
            today
          )
        )
      )
    )
  end apply

  private def renderTableHeader(
      weekDaysS: Signal[List[Date]],
      today: Date
  ): HtmlElement =
    thead(
      tr(
        th(cls := "gutter"),
        children <-- weekDaysS.split(_.dateToString) { (_, day, _) =>
          th(
            todayMods(day, today),
            span(cls := "day__name", daysOfTheWeekShort(day.getWeekDayIndex)),
            span(cls := "day__date", day.dateToStringShort)
          )
        }
      )
    )
  end renderTableHeader

  private def renderTableBody(
      appointmentsS: Signal[WeekAppointments],
      currentTimeS: Signal[Double],
      inWindowS: Signal[Boolean],
      selectedAppointmentVar: Var[Option[Appointment]],
      today: Date
  ): HtmlElement =
    tbody(
      tr(
        renderGutter(),
        children <-- appointmentsS.split(_.stringKey) {
          (_, dayColumn, daySignal) =>
            td(
              cls := "rail",
              cls("day__empty") <-- daySignal.map(
                _.orderedAppointments.isEmpty
              ),
              todayMods(dayColumn.date, today),
              ul(
                children <-- daySignal
                  .map(_.orderedAppointments)
                  .split(_.appointment.appointmentId) { (_, initialAppt, _) =>
                    AppointmentBlock(
                      initialAppt.appointment,
                      selectedAppointmentVar
                    )
                  },
                if today.sameDayAs(dayColumn.date) then
                  nowLine(currentTimeS, inWindowS)
                else Mod.empty
              )
            )
        }
      )
    )
  end renderTableBody

  private def todayMods(day: Date, today: Date): Mod[HtmlElement] =
    cls("timetable__day--today") := day.sameDayAs(today)
  end todayMods

  private def nowLine(
      currentS: Signal[Double],
      inWindowS: Signal[Boolean]
  ): HtmlElement =
    span(
      cls := "now-line",
      styleProp("--n") <-- currentS,
      hidden <-- inWindowS.map(!_)
    )

  end nowLine

  // The visible day runs from windowStart to windowEnd (hours). The CSS has the same window
  // in --from and --hours: change them together.
  private val windowStart = 8
  private val windowEnd = 18

  // Hour labels every two hours; --i is the offset in hours from the window start.
  private def renderGutter(): HtmlElement =
    td(
      cls := "gutter",
      ol(
        (windowStart to windowEnd by 2).map { hour =>
          li(styleAttr := s"--i: ${hour - windowStart}", f"$hour%02d:00")
        }
      )
    )
  end renderGutter
