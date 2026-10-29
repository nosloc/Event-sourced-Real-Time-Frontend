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
  private[components] val popover = htmlAttr("popover", StringAsIsCodec)
  private[components] val popoverTarget =
    htmlAttr("popovertarget", StringAsIsCodec)
  private[components] val popoverTargetAction =
    htmlAttr("popovertargetaction", StringAsIsCodec)
  private[components] val anchorName = styleProp[String]("anchor-name")
  private[components] val positionAnchor = styleProp[String]("position-anchor")

  /** @param id
    *   unique on the page: it names the popover and its anchor
    * @param label
    *   the small caption inside the closed button
    * @param items
    *   the rows, they may arrive later (empty list until loaded)
    * @param fallbackLabel
    *   what the closed button shows while the selected value is not in `items`
    * @param lockedS
    *   shows the picker as it looks, but it cannot be opened or changed (for a
    *   value that is fixed, like the status of a prestation being created)
    */
  def apply[V](
      id: String,
      label: String,
      items: Signal[List[Item[V]]],
      selectedVar: Var[V],
      fallbackLabel: V => String = (v: V) => v.toString,
      lockedS: Signal[Boolean] = Signal.fromValue(false)
  ): HtmlElement =
    controlled(
      id,
      label,
      items,
      selectedVar.signal,
      selectedVar.writer,
      fallbackLabel,
      lockedS
    )

  /** The same picker, for a value it does not own: it shows `selectedS` and
    * sends each choice to `onSelect`, which decides what happens to it.
    */
  def controlled[V](
      id: String,
      label: String,
      items: Signal[List[Item[V]]],
      selectedS: Signal[V],
      onSelect: Observer[V],
      fallbackLabel: V => String = (v: V) => v.toString,
      lockedS: Signal[Boolean] = Signal.fromValue(false)
  ): HtmlElement =
    val listId = s"picker-list-$id"
    val anchor = s"--picker-$id"

    val buttonLabelS: Signal[String] =
      items.combineWith(selectedS).map { case (rows, selected) =>
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
        disabled <-- lockedS,
        span(cls := "picker__label", label),
        span(cls := "picker__value", child.text <-- buttonLabelS)
      ),
      // A popover stays hidden until its button opens it, and a locked
      // picker's button is disabled, so the list can always be there
      div(
        idAttr := listId,
        cls := "picker__list",
        positionAnchor := anchor,
        popover := "auto",
        children <-- items.map(_.map { item =>
          button(
            typ := "button",
            cls := "picker__item",
            cls("picker__item--selected") <-- selectedS
              .map(_ == item.value),
            popoverTarget := listId,
            popoverTargetAction := "hide",
            onClick.mapTo(item.value) --> onSelect,
            span(cls := "picker__name", item.name),
            item.hint.map(h => span(cls := "picker__id", s"($h)"))
          )
        })
      )
    )
