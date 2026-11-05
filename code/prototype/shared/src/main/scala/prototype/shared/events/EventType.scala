package prototype.shared.events

import scala.reflect.ClassTag

import io.circe.{parser, Decoder}

/** The type name an event is stored under, and how to read it back. P4S stores
  * an event under its simple class name (`EventTypeName.of`); P4S is on the JVM
  * only, so the same rule is written here, for the browser too. Renaming an
  * event class renames its type: the events already stored would then need an
  * alias on the server.
  */
object EventType:

  /** How to read each event of a family, by type name */
  type Decoders[E] = Map[String, String => Either[io.circe.Error, E]]

  /** The type name of E, for example "AppointmentCreated" */
  def nameOf[E <: DomainEvent](using ct: ClassTag[E]): String =
    ct.runtimeClass.getSimpleName

  /** One entry of a decoder table: the type name of E, and how to read its
    * payload
    */
  def decoder[E <: DomainEvent: ClassTag: Decoder]
      : (String, String => Either[io.circe.Error, E]) =
    nameOf[E] -> (payload => parser.decode[E](payload))
end EventType
