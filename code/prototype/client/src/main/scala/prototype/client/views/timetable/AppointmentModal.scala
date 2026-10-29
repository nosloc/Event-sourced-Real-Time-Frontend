package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.components.Modal
import prototype.client.views.timetable.AppointmentForm.AppointmentFormState
import prototype.client.ErrorMessages

object AppointmentModal:
  def apply(
      selectedAppointmentIdVar: Var[Option[AppointmentId]],
      selectedAppointmentS: Signal[Option[Appointment]],
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]]
  ): Mod[HtmlElement] =
    child.maybe <-- selectedAppointmentS
      .split(_.appointmentId) { (apptId, _, apptS) =>
        {
          val editingVar: Var[Boolean] = Var(false)

          // Var holding whether the prestations view is being shown
          val showPrestationsVar: Var[Boolean] = Var(false)

          // Signal indicating whether the modal is in normal mode
          val showNormalRowsS: Signal[Boolean] =
            showPrestationsVar.signal
              .combineWith(editingVar.signal)
              .map {
                case (false, false) => true
                case _              => false
              }
              .distinct

          // Signal holding wether the modal is in edit mode
          val showEditFormS: Signal[Boolean] =
            showPrestationsVar.signal
              .combineWith(editingVar.signal)
              .map {
                case (false, true) => true
                case _             => false
              }
              .distinct

          val titleS: Signal[String] =
            editingVar.signal.combineWith(showPrestationsVar).map {
              case (_, true)  => "Appointment's prestations"
              case (true, _)  => "Edit Appointment"
              case (false, _) => "Appointment details"
            }

          Modal(
            titleS = titleS,
            onClose = () => {
              selectedAppointmentIdVar.set(None)
            }
          )(
            child.maybe <-- showNormalRowsS.map {
              Option.when(_)(
                renderRows(
                  selectedAppointmentIdVar,
                  apptId,
                  apptS,
                  editingVar,
                  showPrestationsVar,
                  therapistListS,
                  patientListS
                )
              )
            },
            // When in edit mode no real time changes are reflected directly, so we use the withCurrentValueOf to get the appointment at the moment of entering edit mode
            child.maybe <-- showEditFormS.withCurrentValueOf(apptS).map {
              case (true, appt) =>
                Some(
                  renderEditForm(
                    editingVar,
                    appt,
                    therapistListS,
                    patientListS,
                    // Only what the form edits: a prestation added
                    // meanwhile is not overwritten by saving
                    apptS.map(current =>
                      editedFields(current) != editedFields(appt)
                    )
                  )
                )
              case _ => None
            },
            child.maybe <-- showPrestationsVar.signal.map {
              Option.when(_)(
                PrestationsView(
                  apptId,
                  apptS,
                  showPrestationsVar
                )
              )
            }
          )
        }
      }
  end apply

  // What the edit form can change
  private def editedFields(appt: Appointment) =
    (appt.timeRange, appt.appointmentType, appt.therapistIds, appt.patientIds)

  private def renderRows(
      selectedAppointmentIdVar: Var[Option[AppointmentId]],
      apptId: AppointmentId,
      apptS: Signal[Appointment],
      editingVar: Var[Boolean],
      showPrestationsVar: Var[Boolean],
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]]
  ): HtmlElement =
    div(
      appointmentDetailsRow(
        "Date",
        apptS.map(_.timeRange.start.toDate.dateToString)
      ),
      appointmentDetailsRow(
        "Start Time",
        apptS.map(_.timeRange.start.toDate.getTimeString)
      ),
      appointmentDetailsRow(
        "End Time",
        apptS.map(_.timeRange.end.toDate.getTimeString)
      ),
      appointmentDetailsRow(
        "Type",
        apptS.map(
          _.appointmentType.stringValue.toLowerCase
            .replace("_", " ")
            .capitalize
            .toString
        )
      ),
      appointmentDetailsListRow(
        "Therapists",
        therapistListS
          .combineWith(apptS)
          .map((therapists, appt) =>
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
        patientListS
          .combineWith(apptS)
          .map((patients, appt) =>
            appt.patientIds.map(id =>
              patients
                .find(_.id == id)
                .map(p => s"${p.firstName} ${p.lastName}")
                .getOrElse(id.value.toString)
            )
          ),
        emptyText = "None"
      ),
      renderModalActions(
        apptId,
        selectedAppointmentIdVar,
        editingVar,
        showPrestationsVar
      )
    )
  end renderRows

  private def appointmentDetailsRow(
      label: String,
      value: Signal[String]
  ): HtmlElement =
    div(
      cls := "modal__row",
      span(cls := "modal__label", label),
      span(cls := "modal__value", text <-- value)
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
      appointmentId: AppointmentId,
      selectedAppointmentIdVar: Var[Option[AppointmentId]],
      editingVar: Var[Boolean],
      prestationsVar: Var[Boolean]
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
        "Prestations",
        onClick --> { _ => prestationsVar.set(true) }
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
              Api.appointments.deleteAppointment(appointmentId)
            )
            .stateS
        )
          --> {
            case Api.Loading =>
              loadingVar.set(true)
              errorVar.set(None)
            case Api.Success(_) =>
              loadingVar.set(false)
              selectedAppointmentIdVar.set(None)
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
      editingVar: Var[Boolean],
      appt: Appointment,
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]],
      changesHappenedS: Signal[Boolean]
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
        editingVar.set(false)
      },
      onCancel = () => editingVar.set(false),
      submitLabel = "Submit",
      therapistListS = therapistListS,
      patientListS = patientListS,
      changesHappenedS = changesHappenedS
    )
  end renderEditForm
