package prototype.client.patients

import prototype.api.EventMessage
import prototype.shared.patient.PatientEvent

import org.scalajs.dom

/** The browser side of `PatientEvent.decode`: a patient event that does not decode
  * is reported in the console, and skipped.
  */
object PatientEventDecoder:
  def decode(message: EventMessage): Option[PatientEvent] =
    PatientEvent.decode(message).flatMap {
      case Right(event) => Some(event)
      case Left(error)  =>
        dom.console.warn(
          s"Event #${message.position} (${message.eventType}) not decoded: ${error.getMessage}"
        )
        None
    }
