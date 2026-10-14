package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import cats.effect.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import scala.scalajs.js.Date
import smithy4s.time.Timestamp
import prototype.client.views.components.PopoverPicker

object AppointmentForm:
  final case class AppointmentFormState(
      date: String,
      start: String,
      end: String,
      appointmentType: AppointmentType,
      showErrors: Boolean = false
  ):
    private def parse(d: String, t: String): Option[Date] =
      val parsed = new Date(s"${d}T$t")
      if parsed.getTime().isNaN() then None else Some(parsed)

    // A time string is checked on its own, against a fixed date, so that an
    // empty date does not also flag the times
    private def validTime(t: String): Boolean =
      parse("2000-01-01", t).isDefined

    def dateError: Option[String] =
      Option.when(parse(date, "00:00").isEmpty)("Pick a date")

    def startError: Option[String] =
      Option.when(!validTime(start))("Pick a start time")

    def endError: Option[String] =
      if !validTime(end) then Some("Pick an end time")
      else if parse(date, start).isDefined && toTimeRange.isEmpty then
        Some("End must be after start")
      else None

    private def toTimestamp(d: Date): Timestamp =
      Timestamp.fromEpochMilli(d.getTime().toLong)

    val toTimeRange: Option[TimeRange] =
      for
        startDate <- parse(date, start)
        endDate <- parse(date, end)
        if endDate.getTime() > startDate.getTime()
      yield TimeRange(toTimestamp(startDate), toTimestamp(endDate))

    def displayError(
        error: AppointmentFormState => Option[String]
    ): Option[String] =
      error(this).filter(_ => showErrors)

  def apply[R](
      initialState: AppointmentFormState,
      saveFunction: (TimeRange, AppointmentType) => IO[R],
      onSuccess: () => Unit,
      onCancel: () => Unit,
      submitLabel: String
  ): HtmlElement =

    val loadingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)
    val stateVar: Var[AppointmentFormState] = Var(initialState)

    form(
      onSubmit.preventDefault --> { _ =>
        stateVar.update(_.copy(showErrors = true))
      },
      onSubmit.preventDefault.compose(
        _.sample(
          stateVar.signal
        ).map(s => (s.toTimeRange, s.appointmentType))
          .collect { case (Some(range), apptType) => (range, apptType) }
          .flatMapSwitch { (range, apptType) =>
            ApiCall
              .fromIOCallSimple(
                saveFunction(range, apptType)
              )
              .stateS
          }
      ) --> {
        case Api.Loading =>
          loadingVar.set(true)
          errorVar.set(None)
        case Api.Success(_) =>
          loadingVar.set(false)
          onSuccess()
        case Api.Error(error) =>
          loadingVar.set(false)
          errorVar.set(Some(ErrorMessages.errorToMessage(error)))
      },
      renderRow(
        "Date",
        "date",
        stateVar.zoomLazy(_.date)((state, d) => state.copy(date = d)),
        stateVar,
        _.dateError
      ),
      // Start and end side by side
      div(
        cls := "form__pair",
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
        )
      ),
      renderDropdownRow[AppointmentType](
        selectedVar = stateVar.zoomLazy(_.appointmentType)((state, t) =>
          state.copy(appointmentType = t)
        ),
        labelText = "Type",
        popoverText = "",
        popoverId = "appointment-type-picker",
        items = Signal.fromValue(
          AppointmentType.values.map { t =>
            PopoverPicker.Item(
              value = t,
              name = t.name.toLowerCase.replace("_", " ").capitalize,
              hint = None
            )
          }
        ),
        fallbackLabel = _.name
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
          onClick --> { _ => onCancel() }
        ),
        button(
          typ := "submit",
          cls := "btn btn--primary",
          submitLabel,
          disabled <-- loadingVar.signal
        )
      )
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

  private def renderDropdownRow[V](
      selectedVar: Var[V],
      labelText: String,
      popoverText: String,
      popoverId: String,
      items: Signal[List[PopoverPicker.Item[V]]],
      fallbackLabel: V => String
  ): HtmlElement =
    label(
      cls := "form__label",
      span(labelText),
      PopoverPicker(
        id = popoverId,
        label = popoverText,
        items = items,
        selectedVar = selectedVar,
        fallbackLabel = fallbackLabel
      )
    )
