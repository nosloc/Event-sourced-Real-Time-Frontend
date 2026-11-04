package prototype.shared.therapist

import java.util.UUID

import prototype.api.{CompanyId, Role, Therapist, TherapistId}

/** A therapist as the projection stores it and the browser folds it (see
  * `TherapistEvolve`). Plain ids: the smithy wrappers (`TherapistId`,
  * `CompanyId`) are not stored; the role is stored by its name (see
  * `TherapistRepository`).
  */
final case class TherapistState(
    therapistId: String,
    firstName: String,
    lastName: String,
    companyId: UUID,
    role: Role
)

/** The state and the API's `Therapist` are the same data in two shapes: the
  * state uses plain ids (as stored and folded), the API its smithy wrappers
  * (`TherapistId`, `CompanyId`).
  */
object TherapistState:

  def toTherapist(state: TherapistState): Therapist =
    Therapist(
      firstName = state.firstName,
      lastName = state.lastName,
      therapistId = TherapistId(state.therapistId),
      companyId = CompanyId(state.companyId),
      role = state.role
    )

  def fromTherapist(therapist: Therapist): TherapistState =
    TherapistState(
      therapistId = therapist.therapistId.value,
      firstName = therapist.firstName,
      lastName = therapist.lastName,
      companyId = therapist.companyId.value,
      role = therapist.role
    )
