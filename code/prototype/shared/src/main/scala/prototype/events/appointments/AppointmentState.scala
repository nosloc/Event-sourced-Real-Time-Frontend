package prototype.events.appointments

import java.util.UUID
import prototype.api.PrestationStatus
/*
 * The state the appointment events fold into (see `AppointmentEvolve`), the
 * same on both sides. The server's projection stores it (see
 * `AppointmentRepository`) and the service turns it into the API's
 * `Appointment`; the browser will fold the events into it too, then map it to
 * `Appointment`.
 */

/** A prestation of an appointment, stored in its own table. */
final case class PrestationRow(
    prestationId: UUID,
    label: String,
    unitPrice: Float,
    quantity: Int,
    status: PrestationStatus
)

/** An appointment with its prestations, as the projection sees it. It is stored
  * in two tables (see `AppointmentRepository`). Plain types only: the smithy
  * types (`AppointmentId`, `TimeRange`, ...) are not stored. The two ends of
  * the time range are epoch milliseconds, like in the events.
  */
final case class AppointmentState(
    appointmentId: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    startTime: Long,
    endTime: Long,
    prestations: List[PrestationRow]
)
