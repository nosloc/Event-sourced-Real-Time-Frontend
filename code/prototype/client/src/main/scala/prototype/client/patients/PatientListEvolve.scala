package prototype.client.patients

import prototype.api.Patient
import prototype.shared.patient.{PatientEvent, PatientEvolve, PatientState}

import org.scalajs.dom

/** The patients: applies `PatientEvolve` to the patient an event is about.
  * Patients can only be created today; the full update / delete handling is kept
  * so the list follows the fold if events are added.
  */
object PatientListEvolve:
  def evolve(
      patients: List[Patient],
      event: PatientEvent
  ): List[Patient] =
    val current = patients.find(_.id.value == event.id)
    val evolveResult = PatientEvolve
      .evolve(
        current.map(PatientState.fromPatient),
        event
      )
      .map(_.map(PatientState.toPatient))

    evolveResult match
      case Left(error) =>
        dom.console.warn(
          s"Patient event skipped, it does not fit the patients held: ${error.getMessage}"
        )
        patients // The event does not fit the current state: ignore it, keep the current state
      case Right(evolved) =>
        updatePatients(
          patients,
          event,
          current,
          evolved
        )
  end evolve

  private def updatePatients(
      patients: List[Patient],
      event: PatientEvent,
      current: Option[Patient],
      evolved: Option[Patient]
  ): List[Patient] =
    evolved match
      case Some(updated) if current.isDefined =>
        patients.map(x =>
          if x.id.value == event.id then updated
          else x
        )
      case Some(created) =>
        created :: patients
      case None if current.isDefined =>
        patients.filterNot(_.id.value == event.id)
      case None =>
        patients
  end updatePatients
