package prototype.client.Views

import com.raquo.laminar.api.L.*
import scala.scalajs.js.Date
import prototype.api.*
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.Pages.Page
import prototype.client.AppRouter
import prototype.client.Session
import prototype.client.Pages

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

    div(
      h1("Timetable"),
      b("Welcome to the timetable page!"),
      p("You are logged in as: ", username),
      renderUserInfo(currentUserInfos),
      renderDateRangePicker(weekRangeS, weekOffsetVar),
      renderTimetable(appointmentsForCurrentWeekS, allAppointments.errorS),
      renderNavigationButton(
        "Go to Home",
        Pages.HomePage,
        replaceState = false
      ),
      logoutButton()
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
      child <-- currentUser.stateS.map {
        case Api.Loading =>
          div("Loading user info...")
        case Api.Success(therapist) =>
          div(
            span(s" ${therapist.firstName} ${therapist.lastName}"),
            span(s" - ${therapist.role.toString().toLowerCase()} "),
            span(s"(${therapist.therapistId.value})")
          )
        case Api.Error(error) =>
          div(color.red, s"Error loading user info: ${error.getMessage}")
      }
    )
  end renderUserInfo

  private def renderDateRangePicker(
      weekRangeS: Signal[WeekRange],
      weekOffsetVar: Var[Int]
  ): HtmlElement =
    div(
      backgroundColor.lime,
      div(
        renderDateRange(weekRangeS.map(_.monday), weekRangeS.map(_.sunday)),
        changingWeekButton("Previous Week", weekOffsetVar, -1),
        changingWeekButton("Next Week", weekOffsetVar, 1)
      )
    )
  end renderDateRangePicker

  private def renderDateRange(
      mondayS: Signal[Date],
      sundayS: Signal[Date]
  ): HtmlElement =
    div(
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
      label,
      onClick --> { _ =>
        offsetVar.update(_ + weekOffset)
      }
    )
  end changingWeekButton

  private def renderTimetable(
      appointmentsS: Signal[WeekAppointments],
      errorS: Signal[Option[Throwable]]
  ): HtmlElement =
    div(
      child.maybe <-- errorS.map {
        case Some(error) =>
          Some(
            div(color.red, s"Error loading appointments: ${error.getMessage}")
          )
        case None => None
      },
      backgroundColor.yellow,
      p("This is where the timetable will be displayed."),
      table(
        renderTableHeader(appointmentsS.map(_.keys.toList)),
        renderTableBody(appointmentsS)
      )
    )
  end renderTimetable

  private def renderTableHeader(weekDayS: Signal[List[Date]]): HtmlElement =
    thead(
      tr(
        children <-- weekDayS.map { weeksDay =>
          weeksDay.sortBy(identity).zip(daysOfTheWeekShort).map {
            (day, shortName) =>
              th(s"$shortName: ${day.dateToStringShort}")
          }
        }
      )
    )
  end renderTableHeader

  private def renderTableBody(
      appointmentsS: Signal[WeekAppointments]
  ): HtmlElement =
    tbody(
      tr(
        children <-- appointmentsS.map { weekAppointments =>
          val sortedDays = weekAppointments.keys.toList.sorted
          sortedDays.map { day =>
            weekAppointments(day).sortBy(
              _.appointment.timeRange.start.toDate
            ) match
              case Nil =>
                td("No appointments")
              case appointments =>
                td(
                  ul(
                    appointments.map { awd =>
                      li(
                        s"${awd.appointment.timeRange.start.toDate.getTimeString} - ${awd.appointment.timeRange.end.toDate.getTimeString}"
                      )
                    }
                  )
                )

          }
        }
      )
    )

  end renderTableBody

  def renderNavigationButton(
      label: String,
      page: Page,
      replaceState: Boolean
  ): HtmlElement =
    button(
      label,
      AppRouter.navigateTo(page, replaceState)
    )
  def logoutButton(): HtmlElement =
    button(
      "Logout",
      onClick --> { _ =>
        Session.logout()
      }
    )
