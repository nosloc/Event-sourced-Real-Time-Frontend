package prototype.client.views.Timetable

import prototype.api.*
import scala.scalajs.js.Date
import com.raquo.laminar.api.L.*
import prototype.client.views.Timetable.TimetableModel.*
import prototype.client.Api
import prototype.client.Api.ApiCall

object TimetableView:
  def apply(
      username: String
  ): HtmlElement =

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

    val reloadVar: Var[Int] = Var(0)

    // Current User information
    val currentUserInfos: ApiCall[Therapist] =
      ApiCall.fromIOCall(
        Api.therapists.getTherapist(TherapistId(username)),
        _.therapist
      )

    val allAppointments: ApiCall[List[Appointment]] =
      ApiCall(
        reloadVar.signal.flatMapSwitch(_ =>
          ApiCall
            .fromIOCall(
              Api.appointments.listTherapistAppointments(TherapistId(username)),
              _.appointments
            )
            .stateS
        )
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
            val appointmentsForDay = appointmentsWithDate
              .filter { awd =>
                awd.date.sameDayAs(day)
              }
              .sortBy(_.date)
            DayColumn(day.dateToString, day, appointmentsForDay)
          }

        }
    val currentTimeS: Signal[Double] = EventStream
      .periodic(60 * 1000)
      .map(_ => new Date().decimalHours)
      .startWith(new Date().decimalHours)

    val selectedAppointmentVar: Var[Option[Appointment]] = Var(None)

    div(
      cls := "timetable",
      div(
        cls := "timetable__head",
        div(
          cls := "timetable__who",
          h1("Timetable"),
          UserInfo(currentUserInfos)
        ),
        WeekToolbar(weekRangeS, weekOffsetVar)
      ),
      WeekGrid(
        appointmentsForCurrentWeekS,
        allAppointments.errorS,
        allAppointments.loadingS,
        currentTimeS,
        weekDaysS,
        selectedAppointmentVar,
        today
      ),
      AppointmentModal(selectedAppointmentVar, reloadVar)
    )
  end apply

  private def getDayOfCurrentWeek(now: Date, dayIndex: Int): Date =
    val sinceMonday = now.getWeekDayIndex
    new Date(
      now.getFullYear().toInt,
      now.getMonth().toInt,
      now.getDate().toInt + (dayIndex - sinceMonday)
    )
  end getDayOfCurrentWeek
