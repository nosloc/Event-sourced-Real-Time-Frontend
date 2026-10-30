package prototype.events

/** What every event extends. On the JVM it is a P4S event, so the server can
  * store it; the browser has its own version, an empty marker.
  */
trait DomainEvent extends persistent4s.Event
