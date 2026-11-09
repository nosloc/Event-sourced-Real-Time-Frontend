package prototype.shared.events

import scala.reflect.ClassTag

import io.circe.{parser, Decoder}

/** This part replicates how P4S generates the event type name. The type name of
  * an Event is the simple name of its class, for example "AppointmentCreated".
  * Decoders are a map from type name to a function that decodes the payload
  * into the event. nameOf[E] returns the type name of E, for example
  * "AppointmentCreated". decoder[E] returns a tuple of the type name of E and a
  * function that decodes the payload into E.
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
