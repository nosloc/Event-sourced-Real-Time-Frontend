package prototype.server.core.patient.persistence

import java.util.UUID

import cats.effect.{IO, Resource}

import persistent4s.postgres.{DerivedPostgresRepository, PostgresTable}

import skunk.Session

final class PatientRepository private (pool: Resource[IO, Session[IO]])
    extends DerivedPostgresRepository[IO, UUID, PatientState](
      pool,
      PatientRepository.table
    )

object PatientRepository:

  private val table: PostgresTable[UUID, PatientState] =
    PostgresTable
      .derived[PatientState](tableName = "patients")
      .key(_.patientId)

  def make(pool: Resource[IO, Session[IO]]): PatientRepository =
    new PatientRepository(pool)
