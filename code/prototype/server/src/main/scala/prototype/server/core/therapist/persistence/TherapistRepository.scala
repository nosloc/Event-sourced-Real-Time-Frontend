package prototype.server.core.therapist.persistence

import cats.effect.{IO, Resource}

import prototype.api.Role
import prototype.shared.therapist.TherapistState

import persistent4s.postgres.{
  DerivedPostgresRepository,
  PostgresField,
  PostgresTable
}

import skunk.Session
import skunk.codec.all.text

final class TherapistRepository private (pool: Resource[IO, Session[IO]])
    extends DerivedPostgresRepository[IO, String, TherapistState](
      pool,
      TherapistRepository.table
    )

object TherapistRepository:

  // The role is stored as its name, e.g. "PHYSICIAN": a name no `Role` has
  // means the stored data is corrupted
  private given PostgresField[Role] = PostgresField(
    text.eimap(name => Role.fromString(name).toRight(s"Unknown role: $name"))(
      _.stringValue
    )
  )

  private val table: PostgresTable[String, TherapistState] =
    PostgresTable
      .derived[TherapistState](tableName = "therapists")
      .key(_.therapistId)

  def make(pool: Resource[IO, Session[IO]]): TherapistRepository =
    new TherapistRepository(pool)
