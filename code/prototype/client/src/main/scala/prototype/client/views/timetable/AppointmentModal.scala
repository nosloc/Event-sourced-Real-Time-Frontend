package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.components.Modal
import prototype.client.views.timetable.AppointmentForm.AppointmentFormState

object AppointmentModal:
  def apply(
      selectedAppointmentVar: Var[Option[Appointment]],
      reloadVar: Var[Int],
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]]
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
                renderRows(
                  selectedAppointmentVar,
                  reloadVar,
                  appt,
                  editingVar,
                  therapistListS,
                  patientListS
                )
              else
                renderEditForm(
                  selectedAppointmentVar,
                  reloadVar,
                  editingVar,
                  appt,
                  therapistListS,
                  patientListS
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
      editingVar: Var[Boolean],
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]]
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
      appointmentDetailsRow(
        "Type",
        appt.appointmentType.stringValue.toLowerCase
          .replace("_", " ")
          .capitalize
          .toString
      ),
      appointmentDetailsListRow(
        "Therapists",
        therapistListS.map(therapists =>
          appt.therapistIds.map(id =>
            therapists
              .find(_.therapistId == id)
              .map(t => s"${t.firstName} ${t.lastName} (${id.value})")
              .getOrElse(id.value)
          )
        )
      ),
      appointmentDetailsListRow(
        "Patients",
        patientListS.map(patients =>
          appt.patientIds.map(id =>
            patients
              .find(_.id == id)
              .map(p => s"${p.firstName} ${p.lastName}")
              .getOrElse(id.value.toString)
          )
        ),
        emptyText = "None"
      ),
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

  // A row whose value is a list, one entry per line (names are looked up, so
  // the entries arrive as a signal)
  private def appointmentDetailsListRow(
      label: String,
      entriesS: Signal[List[String]],
      emptyText: String = "-"
  ): HtmlElement =
    div(
      cls := "modal__row",
      span(cls := "modal__label", label),
      div(
        cls := "modal__value modal__value--list",
        children <-- entriesS.map {
          case Nil     => List(span(emptyText))
          case entries => entries.map(entry => span(entry))
        }
      )
    )
  end appointmentDetailsListRow

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
      appt: Appointment,
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]]
  ): HtmlElement =
    AppointmentForm(
      initialState = AppointmentFormState(
        date = appt.timeRange.start.toDate.dateToString,
        start = appt.timeRange.start.toDate.getTimeString,
        end = appt.timeRange.end.toDate.getTimeString,
        appointmentType = appt.appointmentType,
        therapistIds = appt.therapistIds,
        patientIds = appt.patientIds
      ),
      saveFunction = draft =>
        Api.appointments.updateAppointment(
          appt.appointmentId,
          draft.appointmentType,
          draft.therapistIds,
          draft.patientIds,
          draft.timeRange
        ),
      onSuccess = () => {
        selectedAppointmentVar.set(None)
        reloadVar.update(_ + 1)
      },
      onCancel = () => editingVar.set(false),
      submitLabel = "Submit",
      therapistListS = therapistListS,
      patientListS = patientListS
    )
  end renderEditForm
