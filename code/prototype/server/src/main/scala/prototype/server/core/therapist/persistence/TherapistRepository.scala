package prototype.server.core.therapist.persistence

import cats.effect.{IO, Resource}

import persistent4s.postgres.{DerivedPostgresRepository, PostgresTable}

import skunk.Session

final class TherapistRepository private (pool: Resource[IO, Session[IO]])
    extends DerivedPostgresRepository[IO, String, TherapistState](
      pool,
      TherapistRepository.table
    )

object TherapistRepository:

  private val table: PostgresTable[String, TherapistState] =
    PostgresTable
      .derived[TherapistState](tableName = "therapists")
      .key(_.therapistId)

  def make(pool: Resource[IO, Session[IO]]): TherapistRepository =
    new TherapistRepository(pool)
