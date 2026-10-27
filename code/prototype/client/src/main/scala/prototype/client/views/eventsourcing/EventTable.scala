package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*
import prototype.api.EventView

/** The events shown, newest first: position, time, type, tags. The row that
  * just arrived is highlighted; clicking a row selects it.
  */
object EventTable:

  def apply(
      eventsS: Signal[List[EventView]],
      arrivedS: Signal[Option[String]],
      emptyTextS: Signal[String],
      onSelect: EventView => Unit
  ): HtmlElement =
    div(
      cls := "admin__scroll",
      table(
        cls := "admin__table event-sourcing__table",
        thead(tr(th("#"), th("Time"), th("Type"), th("Tags"))),
        tbody(
          // Keyed by event id: a new event adds one row, the others stay
          children <-- eventsS.split(_.eventId)((id, event, _) =>
            renderRow(event, arrivedS.map(_.contains(id)), onSelect)
          ),
          child.maybe <-- eventsS
            .combineWith(emptyTextS)
            .map((events, emptyText) =>
              Option.when(events.isEmpty)(
                tr(td(cls := "admin__empty", colSpan := 4, emptyText))
              )
            )
        )
      )
    )
  end apply

  // A row is a button in spirit: it can be reached with the keyboard too
  private def renderRow(
      event: EventView,
      arrivedS: Signal[Boolean],
      onSelect: EventView => Unit
  ): HtmlElement =
    tr(
      cls := "admin__row",
      cls("event-sourcing__row--arrived") <-- arrivedS,
      tabIndex := 0,
      td(s"#${event.position}"),
      td(cls := "event-sourcing__time", EventDetailsModal.timeOf(event)),
      td(event.eventType),
      td(
        cls := "event-sourcing__tags",
        if event.tags.isEmpty then "–" else event.tags.mkString(", ")
      ),
      onClick --> { _ => onSelect(event) },
      onKeyDown.filter(_.key == "Enter") --> { _ => onSelect(event) }
    )
end EventTable
