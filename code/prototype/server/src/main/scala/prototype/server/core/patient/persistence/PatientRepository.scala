package prototype.server.core.patient.persistence

import java.util.UUID

import cats.effect.IO

import prototype.shared.patient.PatientState

import persistent4s.postgres.{
  DerivedPostgresRepository,
  PostgresSessions,
  PostgresTable
}

final class PatientRepository private (sessions: PostgresSessions[IO])
    extends DerivedPostgresRepository[IO, UUID, PatientState](
      sessions,
      PatientRepository.table
    ):

  /** Returns all patients, with the projection position. */
  def allPatients: IO[(List[PatientState], Long)] =
    readWithCursor(PatientProjectionName)(session => query.run(session))
      .map { case (patients, cursor) =>
        (patients, cursor.globalPosition)
      }

object PatientRepository:

  private val table: PostgresTable[UUID, PatientState] =
    PostgresTable
      .derived[PatientState](tableName = "patients")
      .key(_.patientId)

  def make(sessions: PostgresSessions[IO]): PatientRepository =
    new PatientRepository(sessions)
