package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import cats.effect.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import scala.scalajs.js.Date
import smithy4s.time.Timestamp
import prototype.client.views.components.{FormRow, PopoverPicker, PopoverMultiPicker}

object AppointmentForm:
  // Used to represent the form state on save
  final case class AppointmentDraft(
      timeRange: TimeRange,
      appointmentType: AppointmentType,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId]
  )

  final case class AppointmentFormState(
      date: String,
      start: String,
      end: String,
      appointmentType: AppointmentType,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
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
      saveFunction: AppointmentDraft => IO[R],
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]],
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
        ).map(s =>
          s.toTimeRange.map(range =>
            AppointmentDraft(
              range,
              s.appointmentType,
              s.therapistIds,
              s.patientIds
            )
          )
        ).collect { case Some(draft) => draft }
          .flatMapSwitch { draft =>
            ApiCall
              .fromIOCallSimple(
                saveFunction(draft)
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
      renderMultiDropdownRow[TherapistId](
        selectedVar = stateVar.zoomLazy(_.therapistIds)((state, t) =>
          state.copy(therapistIds = t)
        ),
        labelText = "Therapists",
        popoverText = "",
        popoverId = "therapist-picker",
        items = therapistListS.map { ts =>
          ts.map { t =>
            PopoverPicker.Item(
              value = t.therapistId,
              name = s"${t.firstName} ${t.lastName}",
              hint = Some(t.therapistId.value),
              buttonLabel = Some(t.therapistId.value)
            )
          }
        },
        fallbackLabel = _.value
      ),
      renderMultiDropdownRow[PatientId](
        selectedVar = stateVar.zoomLazy(_.patientIds)((state, p) =>
          state.copy(patientIds = p)
        ),
        labelText = "Patients",
        popoverText = "",
        popoverId = "patient-picker",
        items = patientListS.map { ps =>
          ps.map { p =>
            PopoverPicker.Item(
              value = p.id,
              name = s"${p.firstName} ${p.lastName}",
              hint = Some(p.dateOfBirth.toString),
              buttonLabel = Some(s"${p.firstName} ${p.lastName}")
            )
          }
        },
        fallbackLabel = _.value.toString
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

  private def renderRow(
      labelText: String,
      inputType: String,
      valueVar: Var[String],
      stateVar: Var[AppointmentFormState],
      error: AppointmentFormState => Option[String]
  ): HtmlElement =
    // Errors only show after the first submit (see displayError)
    FormRow.input(
      labelText,
      inputType,
      valueVar,
      stateVar.signal.map(_.displayError(error))
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

  private def renderMultiDropdownRow[V](
      selectedVar: Var[List[V]],
      labelText: String,
      popoverText: String,
      popoverId: String,
      items: Signal[List[PopoverPicker.Item[V]]],
      fallbackLabel: V => String
  ): HtmlElement =
    label(
      cls := "form__label",
      span(labelText),
      PopoverMultiPicker(
        id = popoverId,
        label = popoverText,
        items = items,
        selectedVar = selectedVar,
        fallbackLabel = fallbackLabel
      )
    )
