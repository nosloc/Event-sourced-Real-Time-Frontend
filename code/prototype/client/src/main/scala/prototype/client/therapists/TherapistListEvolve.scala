package prototype.client.therapists

import prototype.api.Therapist
import prototype.shared.therapist.{TherapistEvent, TherapistEvolve, TherapistState}

import org.scalajs.dom

/** The therapists: applies `TherapistEvolve` to the therapist an event is about.
  * Therapists can only be created today; the full update / delete handling is kept
  * so the list follows the fold if events are added.
  */
object TherapistListEvolve:
  def evolve(
      therapists: List[Therapist],
      event: TherapistEvent
  ): List[Therapist] =
    val current = therapists.find(_.therapistId.value == event.id)
    val evolveResult = TherapistEvolve
      .evolve(
        current.map(TherapistState.fromTherapist),
        event
      )
      .map(_.map(TherapistState.toTherapist))

    evolveResult match
      case Left(error) =>
        dom.console.warn(
          s"Therapist event skipped, it does not fit the therapists held: ${error.getMessage}"
        )
        therapists // The event does not fit the current state: ignore it, keep the current state
      case Right(evolved) =>
        updateTherapists(
          therapists,
          event,
          current,
          evolved
        )
  end evolve

  private def updateTherapists(
      therapists: List[Therapist],
      event: TherapistEvent,
      current: Option[Therapist],
      evolved: Option[Therapist]
  ): List[Therapist] =
    evolved match
      case Some(updated) if current.isDefined =>
        therapists.map(x =>
          if x.therapistId.value == event.id then updated
          else x
        )
      case Some(created) =>
        created :: therapists
      case None if current.isDefined =>
        therapists.filterNot(_.therapistId.value == event.id)
      case None =>
        therapists
  end updateTherapists
