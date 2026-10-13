package prototype.client.Views.Timetable

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api

object AppointmentModal:
  def apply(
      selectedAppointmentVar: Var[Option[Appointment]],
      reloadVar: Var[Int]
  ): Mod[HtmlElement] =
    child.maybe <-- selectedAppointmentVar.signal.map {
      _.map(appt =>
        div(
          cls := "modal",
          div(
            cls := "modal__dialog",
            div(
              cls := "modal__head",
              h2(cls := "modal__title", "Appointment Details"),
              button(
                cls := "modal__close",
                "×",
                onClick --> { _ =>
                  selectedAppointmentVar.set(None)
                }
              )
            ),
            appointmentDetailsRow(
              "Date",
              appt.timeRange.start.toDate.dateToString
            ),
            appointmentDetailsRow(
              "Start Time",
              appt.timeRange.start.toDate.getTimeString
            ),
            appointmentDetailsRow(
              "End Time",
              appt.timeRange.end.toDate.getTimeString
            ),
            appointmentDetailsRow("Therapist ID", appt.therapistId.value),
            renderModalActions(appt, selectedAppointmentVar, reloadVar)
          ),
          onClick.filter(ev => ev.target == ev.currentTarget) --> { _ =>
            selectedAppointmentVar.set(None)
          },
          documentEvents(_.onKeyDown.filter(_.key == "Escape")) --> { _ =>
            selectedAppointmentVar.set(None)
          }
        )
      )
    }
  end apply

  private def appointmentDetailsRow(label: String, value: String): HtmlElement =
    div(
      cls := "modal__row",
      span(cls := "modal__label", label),
      span(cls := "modal__value", value)
    )
  end appointmentDetailsRow

  private def renderModalActions(
      appointment: Appointment,
      selectedAppointmentVar: Var[Option[Appointment]],
      reloadVar: Var[Int]
  ): HtmlElement =
    val loadingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)
    div(
      cls := "modal__actions",
      child.maybe <-- errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      button(
        cls := "btn",
        "Delete",
        onClick.flatMap(_ =>
          ApiCall
            .fromIOCallSimple(
              Api.appointments.deleteAppointment(appointment.appointmentId)
            )
            .stateS
        )
          --> {
            case Api.Loading =>
              loadingVar.set(true)
              errorVar.set(None)
            case Api.Success(_) | Api.Error(AppointmentNotFound) =>
              loadingVar.set(false)
              selectedAppointmentVar.set(None)
              reloadVar.update(_ + 1)
            case Api.Error(error) =>
              loadingVar.set(false)
              errorVar.set(
                Some(s"Error: ${error.getMessage}")
              )
          },
        disabled <-- loadingVar.signal
      )
    )
  end renderModalActions
