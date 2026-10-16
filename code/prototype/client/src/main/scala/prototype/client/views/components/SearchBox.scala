package prototype.client.views.components

import com.raquo.laminar.api.L.*

/** A text box that writes what is typed into `valueVar`, one keystroke at a
  * time, so a list filtered by it follows the typing.
  */
object SearchBox:
  def apply(valueVar: Var[String], placeholderText: String): HtmlElement =
    input(
      cls := "search",
      typ := "search",
      placeholder := placeholderText,
      controlled(
        value <-- valueVar.signal,
        onInput.mapToValue --> valueVar.writer
      )
    )
end SearchBox
