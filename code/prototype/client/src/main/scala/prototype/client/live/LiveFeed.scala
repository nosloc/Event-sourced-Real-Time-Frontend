package prototype.client.live

import prototype.api.EventMessage
import prototype.shared.StreamOpened

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import smithy4s.Blob
import smithy4s.json.Json

/** The server's stream of new events, open while the element is mounted. Its
  * messages go to one bus, in the order received: `Opened` once the server is
  * subscribed, then one `Event` per stored event, and `Closed` when the
  * connection drops. The browser then reconnects on its own and sends `Opened`
  * again.
  *
  * One feed is shared by every live data of a page, and they can be built at
  * any time: `isOpenS` tells whether the stream is open now, and `openings`
  * fires once at start if it already is, then on each `Opened`, so a live data
  * built after the stream opened still loads.
  */
final class LiveFeed:
  import LiveFeed.*

  // Whether the stream is open, so that a listener that starts after the
  // stream opened can still start as if it had just opened
  private val isOpen: Var[Boolean] = Var(false)
  val isOpenS: Signal[Boolean] = isOpen.signal
  // The real time event bus
  private val messages: EventBus[FeedMessage] = new EventBus[FeedMessage]
  val events: EventStream[FeedMessage] = messages.events

  def bind(): Mod[HtmlElement] =
    onMountUnmountCallbackWithState(
      mount = _ => {
        val source = new dom.EventSource(sseURL)
        source.onmessage = event =>
          Json.read[EventMessage](Blob(event.data.toString)) match
            case Right(message) => messages.emit(FeedMessage.Event(message))
            case Left(error)    =>
              dom.console.warn(
                s"Stream message not decoded: ${error.getMessage}"
              )
        source.addEventListener(
          StreamOpened.EventType,
          _ => {
            isOpen.set(true)
            messages.emit(FeedMessage.Opened)
          }
        )
        source.onerror = _ => {
          isOpen.set(false)
          messages.emit(FeedMessage.Closed)
        }
        source
      },
      unmount = (_, stream) => {
        isOpen.set(false)
        stream.foreach(_.close())
      }
    )
  end bind

  // Fires once at start if the stream is already open, then on each `Opened`
  def openings: EventStream[Unit] =
    EventStream
      .unit()
      .filter(_ => isOpen.now())
      .mergeWith(events.collect { case FeedMessage.Opened =>
        ()
      })
  end openings

end LiveFeed

object LiveFeed:

  enum FeedMessage:
    case Opened
    case Event(message: EventMessage)
    case Closed

  val sseURL = "/api/events/stream"
end LiveFeed
