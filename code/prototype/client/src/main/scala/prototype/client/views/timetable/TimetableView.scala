package prototype.client.views.timetable

import prototype.api.*
import scala.scalajs.js.Date
import com.raquo.laminar.api.L.*
import prototype.client.views.timetable.TimetableModel.*
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.live.LiveFeed
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.appointments.LiveAppointmentsForTherapist

object TimetableView:
  def apply(
      username: String
  ): HtmlElement =

    val feed: EventBus[FeedMessage] = new EventBus[FeedMessage]

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

    val selectedTherapistVar: Var[String] = Var(username)

    val allAppointments =
      LiveAppointmentsForTherapist(feed, selectedTherapistVar.signal)

    // The placeholder only shows when there is nothing to display yet: the
    // first load, or a new therapist. A reload keeps the grid on screen.
    val showSkeletonS: Signal[Boolean] =
      allAppointments.loadingS
        .combineWith(allAppointments.valuesS)
        .map((loading, kept) => loading && kept.isEmpty)

    val appointmentsForCurrentWeekS: Signal[WeekAppointments] =
      allAppointments.valuesS
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

    val selectedAppointmentIdVar: Var[Option[AppointmentId]] = Var(None)

    val selectedAppointmentS: Signal[Option[Appointment]] =
      selectedAppointmentIdVar.signal.combineWith(allAppointments.valuesS).map {
        case (Some(selectedId), Some(appointments)) =>
          appointments.find(_.appointmentId == selectedId)
        case _ => None
      }

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
      LiveFeed.bind(feed),
      // The selected appointment left the list (deleted, or no longer the
      // therapist's): forget it, so the modal does not come back by itself
      selectedAppointmentS.changes.filter(_.isEmpty).mapTo(None)
        --> selectedAppointmentIdVar,
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
        selectedAppointmentIdVar,
        today
      ),
      AppointmentModal(
        selectedAppointmentIdVar,
        selectedAppointmentS,
        therapistsVar.successS.map(_.getOrElse(Nil)),
        patientsVar.successS.map(_.getOrElse(Nil))
      ),
      CreateAppointmentModal(
        today,
        username,
        showCreateAppointmentModalVar,
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
