package prototype.server.core.appointment.domain

import java.util.UUID

import persistent4s.Scope

object AppointmentScopes:
  val Appointment: Scope[UUID] = Scope[UUID]("appointment")
  val Prestation: Scope[UUID] = Scope[UUID]("prestation")
