package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import scala.scalajs.js.Date
import prototype.api.*
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.views.timetable.TimetableModel.AppointmentFormState

object CreateAppointmentModal:
  def apply(
      today: Date,
      username: String,
      showCreateAppointmentModalVar: Var[Boolean],
      reloadVar: Var[Int]
  ): Mod[HtmlElement] =
    child.maybe <-- showCreateAppointmentModalVar.signal.map(isOpen =>
      Option.when(isOpen) {
        // Fresh state each time the modal opens
        val stateVar: Var[AppointmentFormState] = Var(
          AppointmentFormState(
            date = today.dateToString,
            start = "09:00",
            end = "10:00"
          )
        )
        val loadingVar: Var[Boolean] = Var(false)
        val errorVar: Var[Option[String]] = Var(None)

        Modal(
          title = "Create Appointment",
          onClose = () => showCreateAppointmentModalVar.set(false)
        )(
          form(
            onSubmit.preventDefault --> { _ =>
              stateVar.update(_.copy(showErrors = true))
            },
            onSubmit.preventDefault.compose(
              _.sample(
                stateVar.signal
              ).map(_.toTimeRange)
                .collect { case Some(range) => range }
                .flatMapSwitch { range =>
                  ApiCall
                    .fromIOCallSimple(
                      Api.appointments.createAppointment(
                        TherapistId(username),
                        range
                      )
                    )
                    .stateS
                }
            ) --> {
              case Api.Loading =>
                loadingVar.set(true)
                errorVar.set(None)
              case Api.Success(_) =>
                loadingVar.set(false)
                showCreateAppointmentModalVar.set(false)
                reloadVar.update(_ + 1)
              case Api.Error(error) =>
                loadingVar.set(false)
                errorVar.set(Some(s"Error: ${error.getMessage}"))
            },
            renderRow(
              "Date",
              "date",
              stateVar.zoomLazy(_.date)((state, d) => state.copy(date = d)),
              stateVar,
              _.dateError
            ),
            renderRow(
              "Start",
              "time",
              stateVar.zoomLazy(_.start)((state, s) => state.copy(start = s)),
              stateVar,
              _.startError
            ),
            renderRow(
              "End",
              "time",
              stateVar.zoomLazy(_.end)((state, e) => state.copy(end = e)),
              stateVar,
              _.endError
            ),
            child.maybe <-- errorVar.signal.map(
              _.map(msg => div(cls := "banner banner--error", msg))
            ),
            div(
              cls := "form__actions",
              button(
                typ := "button", // "submit" is the default in HTML
                cls := "btn",
                "Cancel",
                onClick --> { _ => showCreateAppointmentModalVar.set(false) }
              ),
              button(
                typ := "submit",
                cls := "btn btn--primary",
                "Save",
                disabled <-- loadingVar.signal
              )
            )
          )
        )
      }
    )
  end apply

  private def controlledInput(
      inputType: String,
      valueVar: Var[String]
  ): HtmlElement =
    input(
      typ := inputType,
      controlled(
        value <-- valueVar.signal,
        onInput.mapToValue --> valueVar.writer
      )
    )
  end controlledInput

  private def renderRow(
      labelText: String,
      inputType: String,
      valueVar: Var[String],
      stateVar: Var[AppointmentFormState],
      error: AppointmentFormState => Option[String]
  ): HtmlElement =
    // Errors only show after the first submit (see displayError)
    val errorS = stateVar.signal.map(_.displayError(error))
    div(
      cls := "form__row",
      cls("form__row--error") <-- errorS.map(_.nonEmpty),
      label(
        cls := "form__label",
        span(labelText),
        controlledInput(inputType, valueVar)
      ),
      child.maybe <-- errorS.map(_.map(msg => div(cls := "form__error", msg)))
    )
  end renderRow
