package prototype.server.core.company.domain

import java.util.UUID

import persistent4s.Scope

object CompanyScopes:
  val Company: Scope[UUID] = Scope[UUID]("company")

  val CompanyUnique: Scope[Unit] =
    Scope.encoded[Unit]("company-unique")(_ => "all")
