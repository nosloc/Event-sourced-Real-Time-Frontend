package prototype.client.views.components

import com.raquo.laminar.api.L.*

object StatBox:
  def apply(
      statLabel: String,
      valueS: Signal[String]
  ): HtmlElement =
    div(
      cls := "stat",
      span(
        cls := "stat__label",
        statLabel
      ),
      span(
        cls := "stat__value",
        child.text <-- valueS
      )
    )
