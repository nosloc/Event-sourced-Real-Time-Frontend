package prototype.server.core.patient

import cats.effect.*
import prototype.server.eventStore.*
import prototype.api.*
import prototype.server.core.patient.patientModel.*
import smithy4s.time.LocalDate

trait PatientRepository:
  def alreadyExists(
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ): IO[Boolean]
  def getAllPatients(): IO[List[Patient]]

final case class PatientViewer(
    view: Ref[IO, Map[String, Patient]]
) extends Viewer[PatientState, PatientEvent]
    with PatientRepository:

  override def evolve(
      state: PatientState,
      event: PatientEvent
  ): PatientState =
    event match
      case PatientCreated(patientId, firstName, lastName, dob) =>
        Some(Patient(patientId, firstName, lastName, dob))

  override def getState(tag: String): IO[PatientState] =
    view.get.map(_.get(tag))

  override def updateState(
      tag: String,
      events: List[PatientEvent]
  ): IO[PatientState] =
    view
      .updateAndGet(all =>
        val currentState = all.get(tag)
        val newState = events.foldLeft(currentState)(evolve)
        newState match
          case Some(patient) => all.updated(tag, patient)
          case None          => all - tag
      )
      .map(_.get(tag))

  override def alreadyExists(
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  ): IO[Boolean] =
    view.get.map(
      _.values.exists(p =>
        p.firstName == firstName && p.lastName == lastName && p.dateOfBirth == dateOfBirth
      )
    )

  override def getAllPatients(): IO[List[Patient]] =
    view.get.map(
      _.values.toList
        .sortBy(patient => (patient.lastName, patient.firstName))
    )
