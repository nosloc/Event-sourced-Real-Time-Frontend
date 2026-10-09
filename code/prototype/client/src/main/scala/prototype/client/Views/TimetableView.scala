package prototype.client.Views

import com.raquo.laminar.api.L.*
import scala.scalajs.js.Date
import prototype.api.*
import prototype.client.Api
import prototype.client.Api.ApiCall

object TimetableView:

  case class AppointmentWithDate(appointment: Appointment, date: Date)
  case class WeekRange(monday: Date, sunday: Date)
  type WeekAppointments = Map[Date, List[AppointmentWithDate]]

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

  // Non-breaking space: keeps "to 09:45" together when a narrow column wraps
  private val Nbsp = "\u00a0"

  val daysOfTheWeekShort: List[String] =
    List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  extension (d: Date)
    def dateToString: String =
      val y = d.getFullYear().toInt
      val m = d.getMonth().toInt + 1
      val day = d.getDate().toInt
      f"$y%04d-$m%02d-$day%02d"
    def dateToStringShort: String =
      val m = d.getMonth().toInt + 1
      val day = d.getDate().toInt
      f"$m%02d-$day%02d"
    def getTimeString: String =
      val hours = d.getHours().toInt
      val minutes = d.getMinutes().toInt
      f"$hours%02d:$minutes%02d"
    def sameDayAs(other: Date): Boolean =
      d.getFullYear() == other.getFullYear() &&
        d.getMonth() == other.getMonth() &&
        d.getDate() == other.getDate()
    def decimalHours: Double =
      d.getHours().toDouble + (d.getMinutes().toDouble / 60.0)

  given Ordering[Date] = Ordering.by(_.getTime())

  def apply(username: String): HtmlElement =

    // Timetable week
    val today = new Date()
    val weekOffsetVar = Var(0)
    val focusDayS: Signal[Date] = weekOffsetVar.signal.map(offset =>
      new Date(
        today.getFullYear().toInt,
        today.getMonth().toInt,
        today.getDate().toInt + (offset * 7)
      )
    )
    val weekDaysS: Signal[List[Date]] = focusDayS.map { focusDay =>
      (0 to 6).map(dayIndex => getDayOfCurrentWeek(focusDay, dayIndex)).toList
    }

    val weekRangeS: Signal[WeekRange] = weekDaysS.map { weekDays =>
      WeekRange(weekDays.head, weekDays.last)
    }

    // Current User information
    val currentUserInfos: ApiCall[Therapist] =
      ApiCall.fromIOCall(
        Api.therapists.getTherapist(TherapistId(username)),
        _.therapist
      )

    val allAppointments: ApiCall[List[Appointment]] =
      ApiCall.fromIOCall(
        Api.appointments.listTherapistAppointments(TherapistId(username)),
        _.appointments
      )

    val appointmentsForCurrentWeekS: Signal[WeekAppointments] =
      allAppointments.successS
        .combineWith(weekDaysS)
        .map { case (appointments, weekDays) =>
          val appointmentsWithDate = appointments
            .getOrElse(Nil)
            .map { appointment =>
              val appointmentDate = appointment.timeRange.start.toDate
              AppointmentWithDate(appointment, appointmentDate)
            }
          weekDays.map { day =>
            val appointmentsForDay = appointmentsWithDate.filter { awd =>
              awd.date.sameDayAs(day)
            }
            day -> appointmentsForDay
          }.toMap

        }
    val currentTimeS: Signal[Double] = EventStream
      .periodic(60 * 1000)
      .map(_ => new Date().decimalHours)
      .startWith(new Date().decimalHours)

    val inWindowS: Signal[Boolean] = currentTimeS.map { current =>
      current >= windowStart && current < windowEnd
    }.distinct

    div(
      cls := "timetable",
      div(
        cls := "timetable__head",
        div(
          cls := "timetable__who",
          h1("Timetable"),
          renderUserInfo(currentUserInfos)
        ),
        renderDateRangePicker(weekRangeS, weekOffsetVar)
      ),
      renderTimetable(
        appointmentsForCurrentWeekS,
        allAppointments.errorS,
        allAppointments.loadingS,
        currentTimeS,
        inWindowS,
        today
      )
    )
  end apply

  private def getDayOfCurrentWeek(now: Date, dayIndex: Int): Date =
    val sinceMonday = (now.getDay().toInt + 6) % 7
    new Date(
      now.getFullYear().toInt,
      now.getMonth().toInt,
      now.getDate().toInt + (dayIndex - sinceMonday)
    )
  end getDayOfCurrentWeek

  private def renderUserInfo(
      currentUser: ApiCall[Therapist]
  ): Mod[HtmlElement] =
    div(
      cls := "muted",
      child <-- currentUser.stateS.map {
        case Api.Loading =>
          div(cls := "skeleton skeleton--line")
        case Api.Success(therapist) =>
          div(
            cls := "who",
            span(
              cls := "who__name",
              s"${therapist.firstName} ${therapist.lastName}"
            ),
            span(
              cls := "who__role",
              therapist.role.toString().toLowerCase().replace('_', ' ')
            ),
            span(cls := "who__id", therapist.therapistId.value)
          )
        case Api.Error(error) =>
          div(
            cls := "banner banner--error",
            s"Error loading user info: ${error.getMessage}"
          )
      }
    )
  end renderUserInfo

  private def renderDateRangePicker(
      weekRangeS: Signal[WeekRange],
      weekOffsetVar: Var[Int]
  ): HtmlElement =
    div(
      cls := "timetable__toolbar",
      renderDateRange(weekRangeS.map(_.monday), weekRangeS.map(_.sunday)),
      div(
        cls := "btn-group",
        changingWeekButton("‹", weekOffsetVar, -1),
        todayButton(weekOffsetVar),
        changingWeekButton("›", weekOffsetVar, 1)
      )
    )
  end renderDateRangePicker

  private def renderDateRange(
      mondayS: Signal[Date],
      sundayS: Signal[Date]
  ): HtmlElement =
    div(
      cls := "timetable__range",
      child.text <-- mondayS.combineWith(sundayS).map { case (monday, sunday) =>
        s"Week from ${monday.dateToString} to ${sunday.dateToString}"
      }
    )
  end renderDateRange

  private def changingWeekButton(
      label: String,
      offsetVar: Var[Int],
      weekOffset: Int
  ): HtmlElement =
    button(
      cls := "btn btn--icon",
      label,
      onClick --> { _ =>
        offsetVar.update(_ + weekOffset)
      }
    )
  end changingWeekButton

  private def todayButton(offsetVar: Var[Int]): HtmlElement =
    button(
      cls := "btn",
      "Today",
      onClick --> { _ =>
        offsetVar.set(0)
      },
      disabled <-- offsetVar.signal.map(_ == 0)
    )
  end todayButton

  private def renderTimetable(
      appointmentsS: Signal[WeekAppointments],
      errorS: Signal[Option[Throwable]],
      loadingS: Signal[Boolean],
      currentTimeS: Signal[Double],
      inWindowS: Signal[Boolean],
      today: Date
  ): HtmlElement =
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
          renderTableHeader(appointmentsS.map(_.keys.toList), today),
          renderTableBody(appointmentsS, currentTimeS, inWindowS, today)
        )
      )
    )
  end renderTimetable

  private def renderTableHeader(
      weekDayS: Signal[List[Date]],
      today: Date
  ): HtmlElement =
    thead(
      tr(
        th(cls := "gutter"),
        children <-- weekDayS.map { weeksDay =>
          weeksDay.sortBy(identity).zip(daysOfTheWeekShort).map {
            (day, shortName) =>
              th(
                todayMods(day, today),
                span(cls := "day__name", shortName),
                span(cls := "day__date", day.dateToStringShort)
              )
          }
        }
      )
    )
  end renderTableHeader

  private def renderTableBody(
      appointmentsS: Signal[WeekAppointments],
      currentTimeS: Signal[Double],
      inWindowS: Signal[Boolean],
      today: Date
  ): HtmlElement =
    tbody(
      tr(
        renderGutter(),
        children <-- appointmentsS.map { weekAppointments =>
          val sortedDays = weekAppointments.keys.toList.sorted
          sortedDays.map { day =>
            weekAppointments(day).sortBy(
              _.appointment.timeRange.start.toDate
            ) match
              case Nil =>
                td(
                  cls := "rail day__empty",
                  todayMods(day, today),
                  ul(
                    if today.sameDayAs(day) then
                      nowLine(currentTimeS, inWindowS)
                    else Mod.empty
                  )
                )
              case appointments =>
                td(
                  cls := "rail",
                  todayMods(day, today),
                  ul(
                    appointments.map { awd =>
                      renderAppointment(awd.appointment)
                    },
                    if today.sameDayAs(day) then
                      nowLine(currentTimeS, inWindowS)
                    else Mod.empty
                  )
                )
          }
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
  ): Mod[HtmlElement] =
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

  private def renderAppointment(appointment: Appointment): HtmlElement =
    val start = appointment.timeRange.start.toDate
    val end = appointment.timeRange.end.toDate
    val decimalStart =
      start.decimalHours
    val decimalDuration =
      (end.getTime() - start.getTime()).toDouble / (1000.0 * 60.0 * 60.0)
    li(
      cls := "appt",
      cls("appt--short") <-- Signal.fromValue(decimalDuration < 0.5),
      styleAttr := f"--s: $decimalStart%.2f; --d: $decimalDuration%.2f;",
      span(
        cls := "appt__time",
        s"${start.getTimeString} to${Nbsp}${end.getTimeString}"
      )
    )
  end renderAppointment
end TimetableView
