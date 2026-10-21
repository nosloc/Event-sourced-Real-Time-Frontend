package prototype.server.core.therapist.persistence

import java.util.UUID

import cats.effect.IO

import prototype.server.core.PrototypeEvent
import prototype.server.core.therapist.domain.TherapistCreated

import persistent4s.ExactlyOnceEventSourcedProjection

/** A therapist as stored in the read table. Plain types only: the smithy types
  * (`TherapistId`, `CompanyId`, `Role`) cannot be mapped to columns, so the role
  * is kept as its name.
  */
final case class TherapistState(
    therapistId: String,
    firstName: String,
    lastName: String,
    companyId: UUID,
    role: String
)

final case class TherapistProjection(
    protected val repository: TherapistRepository
) extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      String,
      TherapistState
    ]:

  override val name: String = "therapist-projection"

  override protected val eventHandlers = handlers:
    on[TherapistCreated](_.id).create(event =>
      TherapistState(
        event.id,
        event.firstName,
        event.lastName,
        event.companyId,
        event.role.stringValue
      )
    )
