package prototype.client.live

import prototype.api.EventMessage
import prototype.client.Api
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
  // The position of the last event received, -1 before the first one
  val lastPositionS: Signal[Long] =
    events
      .collect { case FeedMessage.Event(message) => message.position }
      .toSignal(-1L)

  // Records the steps of each event, for the step-by-step mode
  val tracer: Tracer = new Tracer

  // While the stream is open, the store's head every few seconds (or the error
  // that kept it from being read); None while the stream is closed
  private val headChecks: EventStream[Option[Either[Throwable, Long]]] =
    isOpenS.flatMapSwitch(open =>
      if open then
        EventStream
          .periodic(StreamCheckIntervalMillis)
          .flatMapSwitch(_ =>
            Api.stream(Api.eventStore.getHeadPosition().map(_.position))
          )
          .map(Some(_))
      else EventStream.fromValue(None)
    )

  // Where the feed is in relation to the store's head, as `StreamStatus`.
  val streamStatusS: Signal[StreamStatus] =
    headChecks
      .withCurrentValueOf(lastPositionS)
      .scanLeft(StreamCheck.initial)((check, next) =>
        val (head, lastReceived) = next
        StreamCheck.step(check, head, lastReceived)
      )
      .map(_.status)

  def bind(): Mod[HtmlElement] =
    onMountUnmountCallbackWithState(
      mount = _ => {
        val source = new dom.EventSource(sseURL)
        source.onmessage = event =>
          Json.read[EventMessage](Blob(event.data.toString)) match
            case Right(message) =>
              tracer.record(
                TraceStep.Kind.Received(message.eventType, message.payload),
                message.position
              )
              messages.emit(FeedMessage.Event(message))
              tracer.record(TraceStep.Kind.Ended, message.position)
            case Left(error) =>
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

  // How often `streamStatusS` reads the store's head
  val StreamCheckIntervalMillis = 2000

  /** Whether the browser keeps up with the store, as `streamStatusS` shows it.
    */
  enum StreamStatus:
    case NotChecked
    case Closed
    case UpToDate(head: Long)
    case Behind(head: Long, received: Long)
    case Ahead(head: Long, received: Long)
    case CheckFailed(error: Throwable, failures: Int, last: StreamStatus)

  /** What the checks remember between two answers. */
  final case class StreamCheck(firstHead: Option[Long], status: StreamStatus)

  object StreamCheck:
    val initial: StreamCheck = StreamCheck(None, StreamStatus.NotChecked)

    def step(
        check: StreamCheck,
        head: Option[Either[Throwable, Long]],
        lastReceived: Long
    ): StreamCheck =
      head match
        // the stream is closed: the first check after it reopens starts
        // from the head again (the events missed meanwhile are not sent, and
        // the sources reload on the reopening)
        case None =>
          StreamCheck(None, StreamStatus.Closed)
        case Some(Left(error)) =>
          val (failures, last) = check.status match
            case StreamStatus.CheckFailed(_, failures, last) =>
              (failures + 1, last)
            case other => (1, other)
          check.copy(status = StreamStatus.CheckFailed(error, failures, last))
        case Some(Right(head)) =>
          val firstHead = check.firstHead.getOrElse(head)
          val received = lastReceived.max(firstHead)
          val status =
            if received == head then StreamStatus.UpToDate(head)
            else if received < head then StreamStatus.Behind(head, received)
            else
              check.status match
                case StreamStatus.Ahead(_, _) =>
                  StreamStatus.Ahead(head, received)
                // Ahead once may be an event stored while the head was read:
                // it counts from the second check in a row
                case previous => previous
          StreamCheck(Some(firstHead), status)
  end StreamCheck
end LiveFeed
