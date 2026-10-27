package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*

/** The events of the event store, live: the newest ones as they are written,
  * and older pages on demand.
  */
object EventSourcingView:

  def apply(): HtmlElement =
    div(
      cls := "event-sourcing",
      h1("Event store"),
      p("The events will be listed here.")
    )
end EventSourcingView
