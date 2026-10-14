package prototype.client.views.components

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec

object PopoverPicker:

  /** One row of the list.
    *
    * @param value
    *   what is written to the selected var when the row is chosen
    * @param name
    *   the main text of the row
    * @param hint
    *   shown in parentheses after the name, for example a trigram
    * @param buttonLabel
    *   what the closed button shows when this row is selected (default: the
    *   name)
    */
  final case class Item[V](
      value: V,
      name: String,
      hint: Option[String] = None,
      buttonLabel: Option[String] = None
  )

  // The popover is a native HTML element, but it is not yet supported by Laminar, so we have to define the attributes ourselves.
  private val popover = htmlAttr("popover", StringAsIsCodec)
  private val popoverTarget = htmlAttr("popovertarget", StringAsIsCodec)
  private val popoverTargetAction =
    htmlAttr("popovertargetaction", StringAsIsCodec)
  private val anchorName = styleProp[String]("anchor-name")
  private val positionAnchor = styleProp[String]("position-anchor")

  /** @param id
    *   unique on the page: it names the popover and its anchor
    * @param label
    *   the small caption inside the closed button
    * @param items
    *   the rows, they may arrive later (empty list until loaded)
    * @param fallbackLabel
    *   what the closed button shows while the selected value is not in `items`
    */
  def apply[V](
      id: String,
      label: String,
      items: Signal[List[Item[V]]],
      selectedVar: Var[V],
      fallbackLabel: V => String = (v: V) => v.toString
  ): HtmlElement =
    val listId = s"picker-list-$id"
    val anchor = s"--picker-$id"

    val buttonLabelS: Signal[String] =
      items.combineWith(selectedVar.signal).map { case (rows, selected) =>
        rows
          .find(_.value == selected)
          .map(row => row.buttonLabel.getOrElse(row.name))
          .getOrElse(fallbackLabel(selected))
      }

    div(
      cls := "picker",
      button(
        typ := "button",
        cls := "picker__button",
        anchorName := anchor,
        popoverTarget := listId,
        span(cls := "picker__label", label),
        span(cls := "picker__value", child.text <-- buttonLabelS)
      ),
      div(
        idAttr := listId,
        cls := "picker__list",
        positionAnchor := anchor,
        popover := "auto",
        children <-- items.map(_.map { item =>
          button(
            typ := "button",
            cls := "picker__item",
            cls("picker__item--selected") <-- selectedVar.signal
              .map(_ == item.value),
            popoverTarget := listId,
            popoverTargetAction := "hide",
            onClick.mapTo(item.value) --> selectedVar.writer,
            span(cls := "picker__name", item.name),
            item.hint.map(h => span(cls := "picker__id", s"($h)"))
          )
        })
      )
    )
