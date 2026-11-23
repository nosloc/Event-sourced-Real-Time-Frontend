package prototype.server.core.company.persistence

import java.util.UUID

import cats.effect.IO

import prototype.shared.company.CompanyState

import persistent4s.postgres.{
  DerivedPostgresRepository,
  PostgresSessions,
  PostgresTable
}

final class CompanyRepository private (sessions: PostgresSessions[IO])
    extends DerivedPostgresRepository[IO, UUID, CompanyState](
      sessions,
      CompanyRepository.table
    ):

  /** Returns all companies, with the projection position. */
  def allCompanies: IO[(List[CompanyState], Long)] =
    readWithCursor(CompanyProjectionName)(session => query.run(session))
      .map { case (companies, cursor) =>
        (companies, cursor.globalPosition)
      }

object CompanyRepository:

  private val table: PostgresTable[UUID, CompanyState] =
    PostgresTable
      .derived[CompanyState](tableName = "companies")
      .key(_.companyId)

  /** Creates a new instance of the CompanyRepository.
    *
    * @param sessions
    *   the database's session pool
    * @return
    *   a new instance of CompanyRepository
    */
  def make(sessions: PostgresSessions[IO]): CompanyRepository =
    new CompanyRepository(sessions)
