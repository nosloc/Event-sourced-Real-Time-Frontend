package prototype.server.core.patient.persistence

import java.time.LocalDate
import java.util.UUID

import cats.effect.IO

import prototype.server.core.PrototypeEvent
import prototype.server.core.patient.domain.PatientCreated

import persistent4s.ExactlyOnceEventSourcedProjection

final case class PatientState(
    patientId: UUID,
    firstName: String,
    lastName: String,
    dateOfBirth: LocalDate
)

final case class PatientProjection(
    protected val repository: PatientRepository
) extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      UUID,
      PatientState
    ]:

  override val name: String = "patient-projection"

  override protected val eventHandlers = handlers:
    on[PatientCreated](_.id).create(event =>
      PatientState(
        event.id,
        event.firstName,
        event.lastName,
        event.dateOfBirth
      )
    )
