package prototype.server.core.patient.persistence

import java.util.UUID

import cats.effect.{IO, Resource}

import prototype.shared.patient.PatientState

import persistent4s.postgres.{DerivedPostgresRepository, PostgresTable}

import skunk.Session

final class PatientRepository private (pool: Resource[IO, Session[IO]])
    extends DerivedPostgresRepository[IO, UUID, PatientState](
      pool,
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

  def make(pool: Resource[IO, Session[IO]]): PatientRepository =
    new PatientRepository(pool)
