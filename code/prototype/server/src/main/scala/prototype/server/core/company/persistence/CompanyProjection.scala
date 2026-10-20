package prototype.server.core.company.persistence

import java.util.UUID

import cats.effect.IO

import prototype.server.core.PrototypeEvent
import prototype.server.core.company.domain.CompanyCreated

import persistent4s.ExactlyOnceEventSourcedProjection

final case class CompanyState(
    companyId: UUID,
    name: String,
    addressStreet: String,
    addressNumber: String,
    addressCity: String,
    addressState: String
)

final case class CompanyProjection(protected val repository: CompanyRepository)
    extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      UUID,
      CompanyState
    ]:

  override val name: String = "company-projection"

  override protected val eventHandlers = handlers:
    on[CompanyCreated](_.id).create(event =>
      CompanyState(
        event.id,
        event.name,
        event.addressStreet,
        event.addressNumber,
        event.addressCity,
        event.addressState
      )
    )
