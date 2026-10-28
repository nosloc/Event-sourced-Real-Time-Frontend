package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*
import prototype.StreamOpened

/** The server's stream of new events, open while the element is mounted. Each
  * message goes to `eventBus`; `opened` fires once the server is subscribed
  * (the `StreamOpened` event), and again after the browser reconnects on its own.
  */
object LiveFeed:

  val sseURL = "/api/events/stream"

  def bind(
      eventBus: EventBus[String],
      opened: EventBus[Unit]
  ): Mod[HtmlElement] =
    onMountUnmountCallbackWithState(
      mount = _ => {
        val source = new org.scalajs.dom.EventSource(sseURL)
        source.onmessage = event => eventBus.emit(event.data.toString)
        source.addEventListener(StreamOpened.EventType, _ => opened.emit(()))
        source
      },
      unmount = (_, stream) => {
        stream.foreach(_.close())
      }
    )
end LiveFeed
