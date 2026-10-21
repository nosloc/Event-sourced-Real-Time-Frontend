// DISABLED 2026-10-21 (Therapist moved to P4S): this file still targets the old
// in-memory therapist service (TherapistLookup). Re-enable it once the
// appointment and dashboard services depend on a trait the tests can stub.
// Tracked in `todo`.
/*
package prototype.server.core.therapist

import cats.effect.*

import prototype.api.*

/** A therapist service that keeps its therapists in memory, for the tests of
  * the services that only need therapists to exist (dashboards). The real one
  * stores events in Postgres and reads from a projection. Like the real one, it
  * refuses an unknown company and an id that is already taken.
  */
final class InMemoryTherapistService private (
    companies: CompanyService[IO],
    therapists: Ref[IO, Map[TherapistId, Therapist]]
) extends TherapistLookup:

  override def listTherapists(): IO[ListTherapistsOutput] =
    therapists.get.map(all =>
      ListTherapistsOutput(all.values.toList.sortBy(_.therapistId.value))
    )

  override def getTherapist(therapistId: TherapistId): IO[GetTherapistOutput] =
    therapists.get.map(_.get(therapistId)).flatMap {
      case Some(therapist) => IO.pure(GetTherapistOutput(therapist))
      case None            => IO.raiseError(TherapistNotFound(therapistId))
    }

  override def getTherapists(ids: List[TherapistId]): IO[List[Therapist]] =
    therapists.get.map(all => ids.distinct.flatMap(all.get))

  override def createTherapist(
      firstName: String,
      lastName: String,
      therapistId: TherapistId,
      companyId: CompanyId,
      role: Role
  ): IO[CreateTherapistOutput] =
    val therapist = Therapist(firstName, lastName, therapistId, companyId, role)
    companies.getCompany(companyId) *>
      therapists
        .modify { all =>
          if all.contains(therapistId) then (all, false)
          else (all + (therapistId -> therapist), true)
        }
        .flatMap(created =>
          if created then IO.pure(CreateTherapistOutput(therapist))
          else IO.raiseError(TherapistAlreadyExists())
        )

object InMemoryTherapistService:
  def apply(companies: CompanyService[IO]): IO[TherapistLookup] =
    Ref
      .of[IO, Map[TherapistId, Therapist]](Map.empty)
      .map(new InMemoryTherapistService(companies, _))
*/
