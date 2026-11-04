package prototype.client.therapists

import prototype.api.EventMessage
import prototype.shared.therapist.TherapistEvent

import org.scalajs.dom

/** The browser side of `TherapistEvent.decode`: a therapist event that does not decode
  * is reported in the console, and skipped.
  */
object TherapistEventDecoder:
  def decode(message: EventMessage): Option[TherapistEvent] =
    TherapistEvent.decode(message).flatMap {
      case Right(event) => Some(event)
      case Left(error)  =>
        dom.console.warn(
          s"Event #${message.position} (${message.eventType}) not decoded: ${error.getMessage}"
        )
        None
    }
