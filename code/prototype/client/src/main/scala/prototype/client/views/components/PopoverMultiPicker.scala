package prototype.client.views.components

import com.raquo.laminar.api.L.*
import prototype.client.views.components.PopoverPicker.*

/** Like `PopoverPicker`, but several values can be picked. The list is the
  * same native `popover`, and it stays open while rows are toggled (it closes
  * on Escape or a click outside). The closed button summarizes the choice.
  *
  * The picker holds no state: it reads and writes `selectedVar`, a list in the
  * order the values were picked.
  *
  * @param emptyLabel
  *   what the closed button shows when nothing is selected
  */
object PopoverMultiPicker:

  def apply[V](
      id: String,
      label: String,
      items: Signal[List[Item[V]]],
      selectedVar: Var[List[V]],
      emptyLabel: String = "None",
      fallbackLabel: V => String = (v: V) => v.toString
  ): HtmlElement =
    val listId = s"picker-list-$id"
    val anchor = s"--picker-$id"

    // The selected values in picked order, named by their row when it is known
    val summaryS: Signal[String] =
      items.combineWith(selectedVar.signal).map { case (rows, selected) =>
        if selected.isEmpty then emptyLabel
        else
          selected
            .map(value =>
              rows
                .find(_.value == value)
                .map(row => row.buttonLabel.getOrElse(row.name))
                .getOrElse(fallbackLabel(value))
            )
            .mkString(", ")
      }

    def toggle(value: V)(selected: List[V]): List[V] =
      if selected.contains(value) then selected.filterNot(_ == value)
      else selected :+ value

    div(
      cls := "picker picker--multi",
      button(
        typ := "button",
        cls := "picker__button",
        anchorName := anchor,
        popoverTarget := listId,
        span(cls := "picker__label", label),
        span(cls := "picker__value", child.text <-- summaryS)
      ),
      div(
        idAttr := listId,
        cls := "picker__list",
        positionAnchor := anchor,
        popover := "auto",
        children <-- items.map(_.map { item =>
          val isSelectedS = selectedVar.signal.map(_.contains(item.value))
          button(
            typ := "button",
            cls := "picker__item",
            cls("picker__item--selected") <-- isSelectedS,
            aria.selected <-- isSelectedS,
            onClick --> { _ => selectedVar.update(toggle(item.value)) },
            span(cls := "picker__check", "✓"),
            span(cls := "picker__name", item.name),
            item.hint.map(h => span(cls := "picker__id", s"($h)"))
          )
        })
      )
    )
