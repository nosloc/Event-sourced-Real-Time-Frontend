package prototype.server.core.patient.domain

import java.util.UUID

import persistent4s.Scope

object PatientScopes:
  val Patient: Scope[UUID] = Scope[UUID]("patient")

  val PatientUnique: Scope[Unit] =
    Scope.encoded[Unit]("patient-unique")(_ => "all")
