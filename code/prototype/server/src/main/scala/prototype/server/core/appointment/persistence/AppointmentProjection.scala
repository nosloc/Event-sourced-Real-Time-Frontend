package prototype.server.core.appointment.persistence

import java.util.UUID

import cats.effect.IO

import fs2.concurrent.Topic

import prototype.events.PrototypeEvent
import prototype.appointment.{
  AppointmentCreated,
  AppointmentDeleted,
  AppointmentEvent,
  AppointmentUpdated,
  PrestationCreated,
  PrestationDeleted,
  PrestationStatusUpdated,
  AppointmentEvolve,
  AppointmentState
}

import persistent4s.ExactlyOnceEventSourcedProjection

val AppointmentProjectionName: String = "appointment-projection"

/** Every appointment event carries the appointment id, so one projection keeps
  * the appointment and its prestations together: the prestation events update
  * the list in the state, and the repository decides how it is stored.
  */
final case class AppointmentProjection(
    protected val repository: AppointmentRepository
) extends ExactlyOnceEventSourcedProjection[
      IO,
      PrototypeEvent,
      UUID,
      AppointmentState
    ]:

  override val name: String = AppointmentProjectionName

  // The server holds every appointment, so an event that does not fit its
  // appointment is an error: the projection fails on it
  private def evolve(
      appointment: Option[AppointmentState],
      event: AppointmentEvent
  ): IO[Option[AppointmentState]] =
    IO.fromEither(AppointmentEvolve.evolve(appointment, event))

  override protected val eventHandlers = handlers:
    // Let the evolve function do the updates
    // TODO: maybe we should handle whole cases in a single line
    on[AppointmentCreated](_.id).handleF(evolve)
    on[AppointmentUpdated](_.id).handleF(evolve)
    on[AppointmentDeleted](_.id).handleF(evolve)
    on[PrestationCreated](_.id).handleF(evolve)
    on[PrestationStatusUpdated](_.id).handleF(evolve)
    on[PrestationDeleted](_.id).handleF(evolve)

object AppointmentProjection:

  /** What the projection publishes after each commit, for the commands that
    * wait for it (`SyncCommandHandler`): the id of the event it processed and
    * the new states of the appointments it touched.
    */
  type Updates =
    Topic[IO, (UUID, Either[Throwable, Map[UUID, Option[AppointmentState]]])]

  def updates: IO[Updates] =
    Topic[IO, (UUID, Either[Throwable, Map[UUID, Option[AppointmentState]]])]
