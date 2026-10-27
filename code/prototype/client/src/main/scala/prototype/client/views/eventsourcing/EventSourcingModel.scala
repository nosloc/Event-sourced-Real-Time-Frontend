package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*

object EventSourcingModel:
  enum EventSourcingPage:
    case LivePage
    case HistoryPage(skip: Long)

  /** The most events one page can hold on the server */
  val MaxEventsSize = 200

  val sseURL = "/api/events/stream"

  def bind(
      eventBus: EventBus[String],
      opened: EventBus[Unit]
  ): Mod[HtmlElement] =
    onMountUnmountCallbackWithState(
      mount = _ => {
        val source = new org.scalajs.dom.EventSource(sseURL)
        source.onmessage = event => eventBus.emit(event.data.toString)
        source.onopen = _ => opened.emit(())
        source
      },
      unmount = (_, stream) => {
        stream.foreach(_.close())
      }
    )
