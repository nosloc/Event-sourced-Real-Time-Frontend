package prototype.client.views.components

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*

/** A labelled form field with its error line underneath. The error signal
  * decides what shows: the form passes its own "only after the first submit"
  * rule in, so every form behaves the same way.
  */
object FormRow:

  def input(
      labelText: String,
      inputType: String,
      valueVar: Var[String],
      errorS: Signal[Option[String]]
  ): HtmlElement =
    row(labelText, controlledInput(inputType, valueVar), errorS)

  // For a field that is not a text input, like a picker
  def pick(
      labelText: String,
      picker: HtmlElement,
      errorS: Signal[Option[String]]
  ): HtmlElement =
    row(labelText, picker, errorS)

  private def row(
      labelText: String,
      field: HtmlElement,
      errorS: Signal[Option[String]]
  ): HtmlElement =
    div(
      cls := "form__row",
      cls("form__row--error") <-- errorS.map(_.nonEmpty),
      label(
        cls := "form__label",
        span(labelText),
        field
      ),
      child.maybe <-- errorS.map(_.map(msg => div(cls := "form__error", msg)))
    )
  end row

  private def controlledInput(
      inputType: String,
      valueVar: Var[String]
  ): HtmlElement =
    // L.input: plain `input` would be this object's own method
    L.input(
      typ := inputType,
      controlled(
        value <-- valueVar.signal,
        onInput.mapToValue --> valueVar.writer
      )
    )
  end controlledInput
end FormRow
