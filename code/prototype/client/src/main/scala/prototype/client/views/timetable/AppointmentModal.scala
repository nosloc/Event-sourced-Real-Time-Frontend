package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.timetable.AppointmentForm.AppointmentFormState

object AppointmentModal:
  def apply(
      selectedAppointmentVar: Var[Option[Appointment]],
      reloadVar: Var[Int]
  ): Mod[HtmlElement] =
    child.maybe <-- selectedAppointmentVar.signal
      .map {
        _.map(appt => {
          val editingVar: Var[Boolean] = Var(false)
          val titleS: Signal[String] = editingVar.signal.map {
            case true  => "Edit Appointment"
            case false => "Appointment details"
          }
          Modal(
            titleS = titleS,
            onClose = () => selectedAppointmentVar.set(None)
          )(
            child <-- editingVar.signal.map(isEditing =>
              if !isEditing then
                renderRows(selectedAppointmentVar, reloadVar, appt, editingVar)
              else
                renderEditForm(
                  selectedAppointmentVar,
                  reloadVar,
                  editingVar,
                  appt
                )
            )
          )
        })
      }
  end apply

  private def renderRows(
      selectedAppointmentVar: Var[Option[Appointment]],
      reloadVar: Var[Int],
      appt: Appointment,
      editingVar: Var[Boolean]
  ): HtmlElement =
    div(
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
      renderModalActions(appt, selectedAppointmentVar, reloadVar, editingVar)
    )
  end renderRows

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
      reloadVar: Var[Int],
      editingVar: Var[Boolean]
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
        "Edit",
        onClick --> { _ => editingVar.set(true) }
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
            case Api.Success(_) =>
              loadingVar.set(false)
              selectedAppointmentVar.set(None)
              reloadVar.update(_ + 1)
            case Api.Error(error) =>
              loadingVar.set(false)
              errorVar.set(
                Some(ErrorMessages.errorToMessage(error))
              )
          },
        disabled <-- loadingVar.signal
      )
    )
  end renderModalActions

  private def renderEditForm(
      selectedAppointmentVar: Var[Option[Appointment]],
      reloadVar: Var[Int],
      editingVar: Var[Boolean],
      appt: Appointment
  ): HtmlElement =
    AppointmentForm(
      initialState = AppointmentFormState(
        date = appt.timeRange.start.toDate.dateToString,
        start = appt.timeRange.start.toDate.getTimeString,
        end = appt.timeRange.end.toDate.getTimeString
      ),
      saveFunction = (range: TimeRange) =>
        Api.appointments.updateAppointment(appt.appointmentId, range),
      onSuccess = () => {
        selectedAppointmentVar.set(None)
        reloadVar.update(_ + 1)
      },
      onCancel = () => editingVar.set(false),
      submitLabel = "Submit"
    )
  end renderEditForm
