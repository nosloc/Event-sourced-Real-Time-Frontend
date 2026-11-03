package prototype.server.core.company.persistence

import java.util.UUID

import cats.effect.{IO, Resource}

import prototype.shared.company.CompanyState

import persistent4s.postgres.{DerivedPostgresRepository, PostgresTable}

import skunk.Session

final class CompanyRepository private (pool: Resource[IO, Session[IO]])
    extends DerivedPostgresRepository[IO, UUID, CompanyState](
      pool,
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
    * @param pool
    *   managed database connection pool
    * @return
    *   a new instance of CompanyRepository
    */
  def make(pool: Resource[IO, Session[IO]]): CompanyRepository =
    new CompanyRepository(pool)
