package prototype.client.views.timetable

import prototype.api.*
import scala.scalajs.js.Date
import com.raquo.laminar.api.L.*
import prototype.client.views.timetable.TimetableModel.*
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

    val selectedTherapistVar: Var[String] = Var(username)

    val allAppointments: ApiCall[List[Appointment]] =
      ApiCall(
        reloadVar.signal
          .combineWith(selectedTherapistVar.signal)
          .flatMapSwitch((_, username) =>
            ApiCall
              .fromIOCall(
                Api.appointments
                  .listTherapistAppointments(TherapistId(username)),
                _.appointments
              )
              .stateS
          )
      )

    val nonFlickeringAppointments: Var[Option[List[Appointment]]] = Var(None)

    // The placeholder only shows when there is nothing to display yet: the
    // first load, or a new therapist. A reload keeps the grid on screen.
    val showSkeletonS: Signal[Boolean] =
      allAppointments.loadingS
        .combineWith(nonFlickeringAppointments.signal)
        .map((loading, kept) => loading && kept.isEmpty)

    val appointmentsForCurrentWeekS: Signal[WeekAppointments] =
      nonFlickeringAppointments.signal
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

    val showCreateAppointmentModalVar: Var[Boolean] = Var(false)

    val therapistsVar: ApiCall[List[Therapist]] =
      ApiCall.fromIOCall(
        Api.therapists.listTherapists(),
        _.therapists
      )

    val patientsVar: ApiCall[List[Patient]] =
      ApiCall.fromIOCall(
        Api.patients.listPatients(),
        _.patients
      )

    div(
      nonFlickeringAppointmentsUpdate(
        nonFlickeringAppointments,
        allAppointments,
        selectedTherapistVar
      ),
      cls := "timetable",
      div(
        cls := "timetable__head",
        h1("Timetable"),
        div(
          cls := "timetable__controls",
          TherapistPicker(therapistsVar, selectedTherapistVar),
          WeekToolbar(weekRangeS, weekOffsetVar, showCreateAppointmentModalVar)
        )
      ),
      WeekGrid(
        appointmentsForCurrentWeekS,
        allAppointments.errorS,
        showSkeletonS,
        currentTimeS,
        weekDaysS,
        selectedAppointmentVar,
        today
      ),
      AppointmentModal(
        selectedAppointmentVar,
        reloadVar,
        therapistsVar.successS.map(_.getOrElse(Nil)),
        patientsVar.successS.map(_.getOrElse(Nil))
      ),
      CreateAppointmentModal(
        today,
        username,
        showCreateAppointmentModalVar,
        reloadVar,
        therapistsVar.successS.map(_.getOrElse(Nil)),
        patientsVar.successS.map(_.getOrElse(Nil))
      )
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

  private def nonFlickeringAppointmentsUpdate(
      unflickeredAppointments: Var[Option[List[Appointment]]],
      allAppointments: ApiCall[List[Appointment]],
      selectedTherapistVar: Var[String]
  ): Mod[HtmlElement] =
    List(
      allAppointments.stateS --> {
        case Api.Success(appointments) =>
          unflickeredAppointments.set(Some(appointments))
        case Api.Error(_) =>
          unflickeredAppointments.set(None)
        case Api.Loading =>
          // Do nothing, keep the previous value to avoid flickering
          ()
      },
      selectedTherapistVar.distinct --> { _ =>
        // When the therapist changes, clear the appointments to avoid showing the previous therapist's appointments
        unflickeredAppointments.set(None)
      }
    )
