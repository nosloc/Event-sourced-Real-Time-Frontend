package prototype.appointment

import java.util.UUID

import prototype.api.*

import smithy4s.time.Timestamp
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
  * in two tables (see `AppointmentRepository`). Plain ids and times: the smithy
  * wrappers (`AppointmentId`, `TimeRange`, ...) are not stored, only the two
  * enums (`AppointmentType`, `PrestationStatus`), stored by name. The two ends
  * of the time range are epoch milliseconds, like in the events.
  */
final case class AppointmentState(
    appointmentId: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: AppointmentType,
    startTime: Long,
    endTime: Long,
    prestations: List[PrestationRow]
)

/** The state and the API's `Appointment` are the same data in two shapes: the
  * state uses plain ids and times (as stored and folded), the API its smithy
  * wrappers.
  */
object AppointmentState:

  def toAppointment(state: AppointmentState): Appointment =
    Appointment(
      AppointmentId(state.appointmentId),
      state.therapistIds.map(TherapistId(_)),
      state.patientIds.map(PatientId(_)),
      state.apptType,
      TimeRange(
        Timestamp.fromEpochMilli(state.startTime),
        Timestamp.fromEpochMilli(state.endTime)
      ),
      state.prestations.map(prestation =>
        Prestation(
          AppointmentId(state.appointmentId),
          PrestationId(prestation.prestationId),
          prestation.label,
          prestation.quantity,
          prestation.unitPrice,
          prestation.status
        )
      )
    )

  def fromAppointment(appointment: Appointment): AppointmentState =
    AppointmentState(
      appointment.appointmentId.value,
      appointment.therapistIds.map(_.value),
      appointment.patientIds.map(_.value),
      appointment.appointmentType,
      appointment.timeRange.start.epochMilli,
      appointment.timeRange.end.epochMilli,
      appointment.prestations.map(prestation =>
        PrestationRow(
          prestation.prestationId.value,
          prestation.label,
          prestation.unitPrice,
          prestation.quantity,
          prestation.status
        )
      )
    )
