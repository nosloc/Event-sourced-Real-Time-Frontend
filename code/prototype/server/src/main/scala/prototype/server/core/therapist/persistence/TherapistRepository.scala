package prototype.server.core.therapist.persistence

import cats.effect.IO

import prototype.api.Role
import prototype.shared.therapist.TherapistState

import persistent4s.postgres.{
  DerivedPostgresRepository,
  PostgresField,
  PostgresSessions,
  PostgresTable
}

import skunk.codec.all.text

final class TherapistRepository private (sessions: PostgresSessions[IO])
    extends DerivedPostgresRepository[IO, String, TherapistState](
      sessions,
      TherapistRepository.table
    ):

  /** Returns all therapists, with the projection position. */
  def allTherapists: IO[(List[TherapistState], Long)] =
    readWithCursor(TherapistProjectionName)(session => query.run(session))
      .map { case (therapists, cursor) =>
        (therapists, cursor.globalPosition)
      }

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

  def make(sessions: PostgresSessions[IO]): TherapistRepository =
    new TherapistRepository(sessions)
