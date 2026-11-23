package prototype.server.core.patient.persistence

import java.util.UUID

import cats.effect.IO

import prototype.shared.events.PrototypeEvent
import prototype.shared.patient.{PatientEvent, PatientEvolve, PatientState}

import persistent4s.ExactlyOnceEventSourcedProjection

val PatientProjectionName: String = "patient-projection"

/** `delay` runs before each event: a read model fault (`App`). */
final case class PatientProjection(
    protected val repository: PatientRepository,
    delay: IO[Unit] = IO.unit
) extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      UUID,
      PatientState
    ]:

  override val name: String = PatientProjectionName

  private val evolve
      : (Option[PatientState], PatientEvent) => IO[Option[PatientState]] =
    (state, event) => delay *> IO.fromEither(PatientEvolve.evolve(state, event))

  override protected val eventHandlers = handlers:
    onFamily[PatientEvent](_.id).handleF(evolve)
