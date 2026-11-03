package prototype.server.core.patient.persistence

import java.util.UUID

import cats.effect.IO

import prototype.shared.events.PrototypeEvent
import prototype.shared.patient.{
  PatientCreated,
  PatientEvent,
  PatientEvolve,
  PatientState
}

import persistent4s.ExactlyOnceEventSourcedProjection

val PatientProjectionName: String = "patient-projection"

final case class PatientProjection(
    protected val repository: PatientRepository
) extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      UUID,
      PatientState
    ]:

  override val name: String = PatientProjectionName

  private val evolve
      : (Option[PatientState], PatientEvent) => IO[Option[PatientState]] =
    (state, event) => IO.fromEither(PatientEvolve.evolve(state, event))

  override protected val eventHandlers = handlers:
    on[PatientCreated](_.id).handleF(evolve)
