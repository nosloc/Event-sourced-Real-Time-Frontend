package prototype.server.core.therapist

import cats.effect.*
import prototype.server.eventStore.*
import prototype.api.*
import prototype.server.core.therapist.therapistModel.*

trait TherapistRepository:
  def getAllTherapists(): IO[List[Therapist]]

final case class TherapistViewer(
    view: Ref[IO, Map[String, Therapist]]
) extends Viewer[TherapistState, TherapistEvent]
    with TherapistRepository:

  override def evolve(
      state: TherapistState,
      event: TherapistEvent
  ): TherapistState =
    event match
      case TherapistCreated(therapistId, firstName, lastName, companyId, role) =>
        Some(Therapist(firstName, lastName, therapistId, companyId, role))

  override def getState(tag: String): IO[TherapistState] =
    view.get.map(_.get(tag))

  override def updateState(
      tag: String,
      events: List[TherapistEvent]
  ): IO[TherapistState] =
    view
      .updateAndGet(all =>
        val currentState = all.get(tag)
        val newState = events.foldLeft(currentState)(evolve)
        newState match
          case Some(therapist) => all.updated(tag, therapist)
          case None            => all - tag
      )
      .map(_.get(tag))

  // Sorted by id: a Map has no stable order, and the list is shown to users
  override def getAllTherapists(): IO[List[Therapist]] =
    view.get.map(_.values.toList.sortBy(_.therapistId.value))
