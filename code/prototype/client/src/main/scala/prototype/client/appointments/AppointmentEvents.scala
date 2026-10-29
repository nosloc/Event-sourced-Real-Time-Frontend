package prototype.client.appointments

import java.util.UUID

import io.circe.{Decoder, parser}
import org.scalajs.dom
import prototype.api.{EventView, PrestationStatus}

// The stored JSON is the status name, e.g. "TO_BILL"
given Decoder[PrestationStatus] = Decoder[String].emap(value =>
  PrestationStatus.values
    .find(_.stringValue == value)
    .toRight(s"Unknown prestation status: $value")
)

/** The events about appointments as the server stores them, decoded in the
  * browser from the live stream. A copy of the server's event classes: same
  * names, same fields, same JSON. On the server, `TherapistAddedToAppointment`
  * is a therapist event; here it sits with the others because it gives a
  * therapist a whole appointment.
  */
sealed trait AppointmentEvent

final case class AppointmentCreated(
    id: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    startTime: Long,
    endTime: Long
) extends AppointmentEvent derives Decoder

// Only what changed: a field is None when it kept its value
final case class AppointmentUpdated(
    id: UUID,
    therapistsAdded: List[String],
    therapistsRemoved: List[String],
    patientsAdded: List[UUID],
    patientsRemoved: List[UUID],
    apptType: Option[String],
    startTime: Option[Long],
    endTime: Option[Long]
) extends AppointmentEvent derives Decoder

final case class AppointmentDeleted(
    id: UUID
) extends AppointmentEvent derives Decoder

final case class PrestationCreated(
    id: UUID, // The appointment id
    prestationId: UUID,
    label: String,
    unitPrice: Float,
    quantity: Int
) extends AppointmentEvent derives Decoder

final case class PrestationStatusUpdated(
    id: UUID, // The appointment id
    prestationId: UUID,
    status: PrestationStatus
) extends AppointmentEvent derives Decoder

final case class PrestationDeleted(
    id: UUID, // The appointment id
    prestationId: UUID
) extends AppointmentEvent derives Decoder

// To the therapist added by an update: the whole appointment, since the update
// itself only lists what changed
final case class TherapistAddedToAppointment(
    id: String, // The trigram of the therapist added
    appointmentId: UUID,
    appointmentTherapists: List[String],
    appointmentPatients: List[UUID],
    appointmentType: String,
    appointmentStartTime: Long,
    appointmentEndTime: Long
) extends AppointmentEvent derives Decoder

object AppointmentEvent:

  /** The appointment event in a stored event, or None for any other event. An
    * appointment event that does not decode is reported in the console.
    */
  def decode(view: EventView): Option[AppointmentEvent] =
    val decoded: Option[Either[io.circe.Error, AppointmentEvent]] =
      view.eventType match
        case "AppointmentCreated" =>
          Some(parser.decode[AppointmentCreated](view.payload))
        case "AppointmentUpdated" =>
          Some(parser.decode[AppointmentUpdated](view.payload))
        case "AppointmentDeleted" =>
          Some(parser.decode[AppointmentDeleted](view.payload))
        case "PrestationCreated" =>
          Some(parser.decode[PrestationCreated](view.payload))
        case "PrestationStatusUpdated" =>
          Some(parser.decode[PrestationStatusUpdated](view.payload))
        case "PrestationDeleted" =>
          Some(parser.decode[PrestationDeleted](view.payload))
        case "TherapistAddedToAppointment" =>
          Some(parser.decode[TherapistAddedToAppointment](view.payload))
        case _ => None
    decoded.flatMap {
      case Right(event) => Some(event)
      case Left(error)  =>
        dom.console.warn(
          s"Event #${view.position} (${view.eventType}) not decoded: ${error.getMessage}"
        )
        None
    }
  end decode
end AppointmentEvent
