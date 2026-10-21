// DISABLED 2026-10-21 (Therapist and Patient moved to P4S): to fix later. The
// appointment and dashboard services now depend on the internal traits
// (TherapistServiceInternal, PatientServiceInternal); this file was adapted to
// them but is left off for now. Tracked in `todo`.
/*
package prototype.server.core.patient

import cats.effect.*

import prototype.api.*
import prototype.server.core.patient.api.PatientServiceInternal

import smithy4s.time.LocalDate

/** A patient service that keeps its patients in memory, for the tests of the
  * services that only need patients to exist (dashboards). The real one stores
  * events in Postgres and reads from a projection. Like the real one, it
  * refuses a second patient with the same name and date of birth.
  */
final class InMemoryPatientService private (
    patients: Ref[IO, Map[PatientId, Patient]]
) extends PatientServiceInternal:

  override def createPatient(
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ): IO[CreatePatientOutput] =
    IO.randomUUID.flatMap { uuid =>
      val id = PatientId(uuid)
      patients
        .modify { all =>
          val taken = all.values.exists(p =>
            p.firstName == firstName && p.lastName == lastName && p.dateOfBirth == dateOfBirth
          )
          if taken then (all, false)
          else (all + (id -> Patient(id, firstName, lastName, dateOfBirth)), true)
        }
        .flatMap(created =>
          if created then IO.pure(CreatePatientOutput(id))
          else IO.raiseError(PatientAlreadyExists())
        )
    }

  override def listPatients(): IO[ListPatientsOutput] =
    patients.get.map(all =>
      ListPatientsOutput(
        all.values.toList.sortBy(p => (p.lastName, p.firstName))
      )
    )

  override def getPatient(patientId: PatientId): IO[GetPatientOutput] =
    patients.get.map(_.get(patientId)).flatMap {
      case Some(patient) => IO.pure(GetPatientOutput(patient))
      case None          => IO.raiseError(PatientNotFound(patientId))
    }

  override def getPatients(ids: List[PatientId]): IO[List[Patient]] =
    patients.get.map(all => ids.distinct.flatMap(all.get))

object InMemoryPatientService:
  def apply(): IO[InMemoryPatientService] =
    Ref
      .of[IO, Map[PatientId, Patient]](Map.empty)
      .map(new InMemoryPatientService(_))
*/
