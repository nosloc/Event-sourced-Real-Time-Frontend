package prototype.client.live

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import prototype.StreamOpened
import prototype.api.EventView
import smithy4s.Blob
import smithy4s.json.Json

/** The server's stream of new events, open while the element is mounted. Its
  * messages go to one bus, in the order received: `Opened` once the server is
  * subscribed, then one `Event` per stored event, and `Closed` when the
  * connection drops. The browser then reconnects on its own and sends `Opened`
  * again.
  */
object LiveFeed:

  enum FeedMessage:
    case Opened
    case Event(view: EventView)
    case Closed

  val sseURL = "/api/events/stream"

  def bind(messages: EventBus[FeedMessage]): Mod[HtmlElement] =
    onMountUnmountCallbackWithState(
      mount = _ => {
        val source = new dom.EventSource(sseURL)
        source.onmessage = event =>
          Json.read[EventView](Blob(event.data.toString)) match
            case Right(view) => messages.emit(FeedMessage.Event(view))
            case Left(error) =>
              dom.console.warn(
                s"Stream message not decoded: ${error.getMessage}"
              )
        source.addEventListener(
          StreamOpened.EventType,
          _ => messages.emit(FeedMessage.Opened)
        )
        source.onerror = _ => messages.emit(FeedMessage.Closed)
        source
      },
      unmount = (_, stream) => {
        stream.foreach(_.close())
      }
    )
end LiveFeed
