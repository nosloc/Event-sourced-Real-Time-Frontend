package prototype.server.core.therapist.persistence

import cats.effect.IO

import prototype.shared.events.PrototypeEvent
import prototype.shared.therapist.{
  TherapistCreated,
  TherapistEvent,
  TherapistEvolve,
  TherapistState
}

import persistent4s.ExactlyOnceEventSourcedProjection

val TherapistProjectionName: String = "therapist-projection"

final case class TherapistProjection(
    protected val repository: TherapistRepository
) extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      String,
      TherapistState
    ]:

  override val name: String = TherapistProjectionName

  private val evolve
      : (Option[TherapistState], TherapistEvent) => IO[Option[TherapistState]] =
    (state, event) => IO.fromEither(TherapistEvolve.evolve(state, event))

  override protected val eventHandlers = handlers:
    on[TherapistCreated](_.id).handleF(evolve)
