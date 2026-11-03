package prototype.client.appointments

import prototype.api.EventMessage
import prototype.shared.appointment.AppointmentEvent

import org.scalajs.dom

/** The browser side of `AppointmentEvent.decode`: an appointment event that
  * does not decode is reported in the console, and skipped.
  */
object AppointmentEventDecoder:

  def decode(message: EventMessage): Option[AppointmentEvent] =
    AppointmentEvent.decode(message).flatMap {
      case Right(event) => Some(event)
      case Left(error)  =>
        dom.console.warn(
          s"Event #${message.position} (${message.eventType}) not decoded: ${error.getMessage}"
        )
        None
    }
