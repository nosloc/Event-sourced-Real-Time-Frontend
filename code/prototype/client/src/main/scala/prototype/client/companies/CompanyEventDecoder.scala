package prototype.client.companies

import prototype.api.EventMessage
import prototype.shared.company.CompanyEvent

import org.scalajs.dom

object CompanyEventDecoder:
  def decode(message: EventMessage): Option[CompanyEvent] =
    CompanyEvent.decode(message).flatMap {
      case Right(event) => Some(event)
      case Left(error)  =>
        dom.console.warn(
          s"Event #${message.position} (${message.eventType}) not decoded: ${error.getMessage}"
        )
        None
    }
