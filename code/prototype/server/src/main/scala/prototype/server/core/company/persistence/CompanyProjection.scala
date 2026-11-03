package prototype.server.core.company.persistence

import java.util.UUID

import cats.effect.IO

import prototype.shared.company.{
  CompanyCreated,
  CompanyEvent,
  CompanyEvolve,
  CompanyState
}
import prototype.shared.events.PrototypeEvent

import persistent4s.ExactlyOnceEventSourcedProjection

final case class CompanyProjection(protected val repository: CompanyRepository)
    extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      UUID,
      CompanyState
    ]:

  override val name: String = "company-projection"

  private val evolve
      : (Option[CompanyState], CompanyEvent) => IO[Option[CompanyState]] =
    (state, event) => IO.fromEither(CompanyEvolve.evolve(state, event))

  override protected val eventHandlers = handlers:
    on[CompanyCreated](_.id).handleF(evolve)
