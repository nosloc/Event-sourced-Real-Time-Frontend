package prototype.events

/** What every event extends. In the browser it is only a marker; on the JVM it
  * is a P4S event, so the server can store it.
  */
trait DomainEvent
