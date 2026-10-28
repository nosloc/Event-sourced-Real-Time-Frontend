package prototype.client.views.timetable

import prototype.api.*
import scala.scalajs.js.Date
import com.raquo.laminar.api.L.*
import prototype.client.views.timetable.TimetableModel.*
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.live.LiveFeed
import smithy4s.Blob
import smithy4s.json.Json
import prototype.client.live.LiveDataSource
import smithy4s.time.Timestamp

object TimetableView:
  def apply(
      username: String
  ): HtmlElement =

    val rawLiveEvents: EventBus[String] = new EventBus[String]
    val subscribed: EventBus[Unit] = new EventBus[Unit]

    val displayedEvent: Var[Option[EventView]] = Var(None)

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

    // val allAppointmentsAndPosition: ApiCall[(List[Appointment], Long)] =
    //   ApiCall(
    //     reloadVar.signal
    //       .combineWith(selectedTherapistVar.signal)
    //       .flatMapSwitch((_, username) =>
    //         ApiCall
    //           .fromIOCall(
    //             Api.appointments
    //               .listTherapistAppointments(TherapistId(username)),
    //             response => (response.appointments, response.globalPosition)
    //           )
    //           .stateS
    //       )
    //   )

    // val allAppointments: ApiCall[List[Appointment]] =
    //   ApiCall(allAppointmentsAndPosition.stateS.map {
    //     case Api.Success((appointments, _)) => Api.Success(appointments)
    //     case Api.Error(e)                   => Api.Error(e)
    //     case Api.Loading                    => Api.Loading
    //   })

    // val globalPositionS: Signal[Long] =
    //   allAppointmentsAndPosition.stateS.map {
    //     case Api.Success((_, position)) => position
    //     case _                          => 0L
    //   }

    // None for a message that is not an appointment event
    def decode(raw: String): Option[AppointmentEvent] =
      Json.read[EventView](Blob(raw)).toOption.flatMap(AppointmentEvent.decode)

    val liveAppointmentsDataSource =
      new LiveDataSource[List[Appointment], AppointmentEvent, String](
        eventBus = rawLiveEvents,
        opened = subscribed,
        dependencies = selectedTherapistVar.signal,
        fetchIO = username =>
          Api.appointments
            .listTherapistAppointments(TherapistId(username))
            .map(response => (response.appointments, response.globalPosition)),
        decode = decode,
        collect = (username, appointments, event) =>
          event match
            case AppointmentCreated(
                  id,
                  therapistIds,
                  patientIds,
                  apptType,
                  startTime,
                  endTime
                ) if therapistIds.contains(username) =>
              // An unknown type leaves the list unchanged
              AppointmentType.values
                .find(_.stringValue == apptType)
                .map(appointmentType =>
                  appointments :+ Appointment(
                    AppointmentId(id),
                    therapistIds.map(TherapistId.apply),
                    patientIds.map(PatientId.apply),
                    appointmentType,
                    // The events hold epoch milliseconds
                    TimeRange(
                      Timestamp.fromEpochMilli(startTime),
                      Timestamp.fromEpochMilli(endTime)
                    ),
                    Nil
                  )
                )
            case _ => Some(appointments)
      )

    val allAppointments = liveAppointmentsDataSource

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
      LiveFeed.bind(rawLiveEvents, subscribed),
      liveAppointmentsDataSource.bind(),
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
      ),
      rawLiveEvents.events --> { raw =>
        displayedEvent.set(
          Json.read[EventView](Blob(raw)).toOption
        )
      }
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
      allAppointments: LiveDataSource[List[
        Appointment
      ], AppointmentEvent, String],
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
