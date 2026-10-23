package prototype.server.core.appointment.persistence

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.syntax.all.*

import prototype.api.PrestationStatus
import prototype.server.core.appointment.domain.given

import persistent4s.postgres.{PostgresAtomicRepository, PostgresRepositoryTable}

import io.circe.Decoder as JsonDecoder
import skunk.*
import skunk.circe.codec.all.json
import skunk.codec.all.*
import skunk.data.Arr
import skunk.syntax.all.*

/** Stores one state, an appointment with its prestations, in two tables:
  * `appointments` and `prestations`. The library calls the three callbacks
  * below in the same transaction as the projection's checkpoint, so the two
  * tables and the checkpoint always agree.
  *
  * Reading is one join: the prestations of an appointment come back in a single
  * column, in their order. A change to the prestations of an appointment
  * rewrites all of its prestation rows, with their `position`.
  */
final class AppointmentRepository private (pool: Resource[IO, Session[IO]])
    extends PostgresAtomicRepository[IO, UUID, AppointmentState](pool):

  import AppointmentRepository.*

  override protected val table
      : PostgresRepositoryTable[IO, UUID, AppointmentState] =
    PostgresRepositoryTable.rows(
      keyOf = _.appointmentId,
      fetch = (session, keys) => session.execute(byIds)(Arr(keys*)),
      delete = deleteStates,
      upsert = upsertStates
    )

  /** Load every appointment with its prestations. No ordering is implied. */
  def all: IO[List[AppointmentState]] =
    useSession(_.execute(everyAppointment))

  /** The appointments a therapist is in, earliest first. */
  def forTherapist(therapistId: String): IO[List[AppointmentState]] =
    useSession(_.execute(ofTherapist)(therapistId))

object AppointmentRepository:

  def make(pool: Resource[IO, Session[IO]]): AppointmentRepository =
    new AppointmentRepository(pool)

  // How one prestation of the aggregated column is read back
  private given JsonDecoder[PrestationRow] =
    JsonDecoder.forProduct5(
      "prestationId",
      "label",
      "unitPrice",
      "quantity",
      "status"
    )(PrestationRow.apply)

  // One row per appointment: its columns, then all of its prestations in one
  // column (an empty list when it has none)
  private val selectWithPrestations: Fragment[Void] = sql"""
    SELECT a.appointment_id, a.therapist_ids, a.patient_ids, a.appt_type,
           a.start_time, a.end_time,
           COALESCE(
             json_agg(
               json_build_object(
                 'prestationId', p.prestation_id,
                 'label', p.label,
                 'unitPrice', p.unit_price,
                 'quantity', p.quantity,
                 'status', p.status
               ) ORDER BY p.position
             ) FILTER (WHERE p.prestation_id IS NOT NULL),
             '[]'
           ) AS prestations
    FROM appointments a
    LEFT JOIN prestations p ON p.appointment_id = a.appointment_id"""

  private val appointmentState: skunk.Decoder[AppointmentState] =
    (uuid *: _text *: _uuid *: text *: int8 *: int8 *: json).emap {
      case (id, therapistIds, patientIds, apptType, start, end, prestations) =>
        prestations
          .as[List[PrestationRow]]
          .left
          .map(error => s"Appointment $id has unreadable prestations: $error")
          .map(rows =>
            AppointmentState(
              id,
              therapistIds.flattenTo(List),
              patientIds.flattenTo(List),
              apptType,
              start,
              end,
              rows
            )
          )
    }

  private val byIds: Query[Arr[UUID], AppointmentState] =
    sql"""$selectWithPrestations
      WHERE a.appointment_id = ANY($_uuid)
      GROUP BY a.appointment_id""".query(appointmentState)

  private val everyAppointment: Query[Void, AppointmentState] =
    sql"""$selectWithPrestations
      GROUP BY a.appointment_id""".query(appointmentState)

  // The library has no "array contains" filter, hence the query
  private val ofTherapist: Query[String, AppointmentState] =
    sql"""$selectWithPrestations
      WHERE $text = ANY(a.therapist_ids)
      GROUP BY a.appointment_id
      ORDER BY a.start_time, a.appointment_id""".query(appointmentState)

  private val upsertAppointment
      : Command[(UUID, Arr[String], Arr[UUID], String, Long, Long)] =
    sql"""
      INSERT INTO appointments
        (appointment_id, therapist_ids, patient_ids, appt_type, start_time, end_time)
      VALUES (${uuid *: _text *: _uuid *: text *: int8 *: int8})
      ON CONFLICT (appointment_id) DO UPDATE SET
        therapist_ids = EXCLUDED.therapist_ids,
        patient_ids = EXCLUDED.patient_ids,
        appt_type = EXCLUDED.appt_type,
        start_time = EXCLUDED.start_time,
        end_time = EXCLUDED.end_time
    """.command

  private val insertPrestation
      : Command[(UUID, Int, UUID, String, Float, Int, String)] =
    sql"""
      INSERT INTO prestations
        (appointment_id, position, prestation_id, label, unit_price, quantity, status)
      VALUES (${uuid *: int4 *: uuid *: text *: float4 *: int4 *: text})
    """.command

  private val deletePrestationsOf: Command[Arr[UUID]] =
    sql"DELETE FROM prestations WHERE appointment_id = ANY($_uuid)".command

  private val deleteAppointmentsOf: Command[Arr[UUID]] =
    sql"DELETE FROM appointments WHERE appointment_id = ANY($_uuid)".command

  // The prestations go with their appointment: the foreign key cascades
  private def deleteStates(session: Session[IO], keys: List[UUID]): IO[Unit] =
    session.execute(deleteAppointmentsOf)(Arr(keys*)).void

  // Each appointment is inserted or updated, then its prestation rows are
  // replaced by the ones of the state, numbered in their order (updating an
  // appointment does not cascade, so the old rows are deleted here)
  private def upsertStates(
      session: Session[IO],
      states: List[AppointmentState]
  ): IO[Unit] =
    states.traverse_ { state =>
      session.execute(upsertAppointment)(
        (
          state.appointmentId,
          Arr(state.therapistIds*),
          Arr(state.patientIds*),
          state.apptType,
          state.startTime,
          state.endTime
        )
      )
    } *>
      session.execute(deletePrestationsOf)(
        Arr(states.map(_.appointmentId)*)
      ) *>
      states.traverse_ { state =>
        state.prestations.zipWithIndex.traverse_ { (prestation, position) =>
          session.execute(insertPrestation)(
            (
              state.appointmentId,
              position,
              prestation.prestationId,
              prestation.label,
              prestation.unitPrice,
              prestation.quantity,
              prestation.status.stringValue
            )
          )
        }
      }
