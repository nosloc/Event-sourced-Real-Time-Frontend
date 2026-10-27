package prototype.client.views.eventsourcing

import scala.scalajs.js
import scala.scalajs.js.Date

import com.raquo.laminar.api.L.*
import prototype.api.EventView
import prototype.client.views.components.Modal
import prototype.client.views.timetable.dateToString

/** Every field of the selected event, its content as indented JSON. The tab
  * owns `selectedVar`: clicking a row sets it, closing the modal clears it.
  */
object EventDetailsModal:

  def apply(selectedVar: Var[Option[EventView]]): Mod[HtmlElement] =
    child.maybe <-- selectedVar.signal.map(
      _.map(event =>
        Modal(
          title = s"${event.eventType} · #${event.position}",
          onClose = () => selectedVar.set(None)
        )(
          div(
            cls := "event-details",
            row("Time", s"${dateOf(event).dateToString} ${timeOf(event)}"),
            row("Event id", event.eventId),
            // One tag per line, as the appointment modal lists its patients
            div(
              cls := "modal__row",
              span(cls := "modal__label", "Tags"),
              span(
                cls := "modal__value modal__value--list",
                if event.tags.isEmpty then List(span("–"))
                else event.tags.map(tag => span(tag))
              )
            ),
            row("Position", s"#${event.position}"),
            p(
              cls := "event-details__note",
              "Position in the store; numbers can skip."
            ),
            pre(cls := "event-details__payload", indented(event.payload))
          )
        )
      )
    )

  /** Local time, to the second */
  def timeOf(event: EventView): String =
    val d = dateOf(event)
    f"${d.getHours().toInt}%02d:${d.getMinutes().toInt}%02d:${d.getSeconds().toInt}%02d"

  private def dateOf(event: EventView): Date =
    new Date(event.recordedAt.epochMilli.toDouble)

  private def row(label: String, value: String): HtmlElement =
    div(
      cls := "modal__row",
      span(cls := "modal__label", label),
      span(cls := "modal__value", value)
    )

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
end EventDetailsModal
