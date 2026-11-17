package prototype.client.views.components

import scala.scalajs.js

import prototype.client.live.TraceStep
import prototype.client.views.components.PopoverPicker.{
  anchorName,
  popover,
  popoverTarget,
  positionAnchor
}

import com.raquo.laminar.api.L.*

/** The card the step-by-step graph opens when an event, a reload or a snapshot
  * is clicked: a native popover anchored on what was clicked, closed by a click
  * elsewhere or Escape. The graph is drawn again on each Next, so an open card
  * closes when the panel steps.
  */
object StepCard:

  // One card for the whole graph: what it shows follows what was clicked
  private val CardId = "step-card"

  /** Something on the graph that opens the card: `anchor` names it (unique on
    * the graph), `open` says what the card must show.
    */
  def clickable(anchor: String, label: String, open: () => Unit)(
      content: HtmlElement
  ): HtmlElement =
    button(
      tpe := "button",
      cls := "stepper__clickable",
      anchorName := s"--$anchor",
      popoverTarget := CardId,
      aria.label := label,
      onClick --> (_ => open()),
      content
    )

  /** The card, anchored on what was clicked last (`anchorS`), showing
    * `contentS`.
    */
  def popoverCard(
      anchorS: Signal[Option[String]],
      contentS: Signal[Option[HtmlElement]]
  ): HtmlElement =
    div(
      idAttr := CardId,
      cls := "stepper__card",
      popover := "auto",
      positionAnchor <-- anchorS.map(_.fold("none")(anchor => s"--$anchor")),
      child.maybe <-- contentS
    )

  /** A card's content: a title, then its sections. */
  def frame(title: String, sections: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "stepper__card-body",
      div(cls := "stepper__card-title", title),
      sections
    )

  /** A few facts, label and value. */
  def facts(rows: (String, String)*): HtmlElement =
    div(
      cls := "stepper__card-facts",
      rows.map((label, value) =>
        div(
          cls := "stepper__card-fact",
          span(cls := "stepper__card-label", label),
          span(cls := "stepper__card-value", value)
        )
      )
    )

  /** The steps an object went through so far, each with its time from the
    * first one.
    */
  def steps(steps: Vector[TraceStep]): HtmlElement =
    val start = steps.headOption.fold(0.0)(_.at)
    section(
      "Steps so far",
      steps.map(step =>
        div(
          cls := "stepper__card-step",
          span(TraceStep.title(step)),
          span(cls := "stepper__card-time", f"+${step.at - start}%.1f ms")
        )
      )
    )

  /** An event's payload, folded until opened, indented when it is JSON. */
  def payload(json: String): HtmlElement =
    detailsTag(
      cls := "stepper__card-payload",
      summaryTag("Payload"),
      pre(indented(json))
    )

  /** A titled part of a card. */
  def section(title: String, content: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "stepper__card-section",
      div(cls := "stepper__card-subtitle", title),
      content
    )

  /** A duration between two times, or what is still awaited. */
  def between(from: Double, to: Option[Double], awaited: String): String =
    to.fold(awaited)(end => f"${end - from}%.1f ms")

  // Two-space indent; shown as it is if it is not JSON
  private def indented(payload: String): String =
    try
      js.JSON
        .stringify(
          js.JSON.parse(payload),
          null: js.Function2[String, js.Any, js.Any],
          2
        )
    catch case _: js.JavaScriptException => payload
