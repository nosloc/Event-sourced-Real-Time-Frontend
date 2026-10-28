package prototype

/** The first message of each event stream, sent once the server is subscribed:
  * from then on, no new event can be missed. A named event, so it does not reach
  * `onmessage`, and its data is not empty, or the browser would drop it.
  */
object StreamOpened:
  val EventType = "StreamOpened"
  def data(after: Long): String = s"Opened stream filtering events after $after"
