package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.client.views.components.Modal

/** Shows every field of the selected row, the id included. The tab owns
  * `selectedVar`: clicking a row sets it, closing the modal clears it.
  */
object DetailsModal:
  def apply[A](
      selectedVar: Var[Option[A]],
      title: String,
      fieldsOf: A => List[(String, String)]
  ): Mod[HtmlElement] =
    child.maybe <-- selectedVar.signal.map(
      _.map(item =>
        Modal(title = title, onClose = () => selectedVar.set(None))(
          div(
            cls := "admin__details",
            fieldsOf(item).map((label, value) =>
              div(
                cls := "modal__row",
                span(cls := "modal__label", label),
                span(cls := "modal__value", value)
              )
            )
          )
        )
      )
    )
end DetailsModal
