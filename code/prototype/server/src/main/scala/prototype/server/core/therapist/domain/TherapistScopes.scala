package prototype.server.core.therapist.domain

import persistent4s.Scope

object TherapistScopes:
  val Therapist: Scope[String] = Scope[String]("therapist")
