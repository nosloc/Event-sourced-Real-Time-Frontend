package prototype.server.core.appointment.persistence

import java.util.UUID

import cats.effect.IO

import fs2.concurrent.Topic

import prototype.api.PrestationStatus
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.domain.{
  AppointmentCreated,
  AppointmentDeleted,
  AppointmentUpdated,
  PrestationCreated,
  PrestationDeleted,
  PrestationStatusUpdated
}

import persistent4s.ExactlyOnceEventSourcedProjection

/** A prestation of an appointment, stored in its own table. */
final case class PrestationRow(
    prestationId: UUID,
    label: String,
    unitPrice: Float,
    quantity: Int,
    status: PrestationStatus
)

/** An appointment with its prestations, as the projection sees it. It is stored
  * in two tables (see `AppointmentRepository`). Plain types only: the smithy
  * types (`AppointmentId`, `TimeRange`, ...) are not stored. The two ends of
  * the time range are epoch milliseconds, like in the events.
  */
final case class AppointmentState(
    appointmentId: UUID,
    therapistIds: List[String],
    patientIds: List[UUID],
    apptType: String,
    startTime: Long,
    endTime: Long,
    prestations: List[PrestationRow]
)

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

  override protected val eventHandlers = handlers:
    on[AppointmentCreated](_.id).create(event =>
      AppointmentState(
        event.id,
        event.therapistIds,
        event.patientIds,
        event.apptType,
        event.startTime,
        event.endTime,
        prestations = Nil
      )
    )

    // The update carries only what changed. The "added to appointment" events
    // are not handled: the update already lists who joined, so the row would
    // name them twice
    on[AppointmentUpdated](_.id).update((state, event) =>
      val therapistIds =
        state.therapistIds.filterNot(event.therapistsRemoved.contains) ++
          event.therapistsAdded
      val patientIds =
        state.patientIds.filterNot(event.patientsRemoved.contains) ++
          event.patientsAdded
      state.copy(
        therapistIds = therapistIds.distinct,
        patientIds = patientIds.distinct,
        apptType = event.apptType.getOrElse(state.apptType),
        startTime = event.startTime.getOrElse(state.startTime),
        endTime = event.endTime.getOrElse(state.endTime)
      )
    )

    on[AppointmentDeleted](_.id).delete

    // A new prestation starts at its first status
    on[PrestationCreated](_.id).update((state, event) =>
      state.copy(prestations =
        state.prestations :+ PrestationRow(
          event.prestationId,
          event.label,
          event.unitPrice,
          event.quantity,
          PrestationStatus.TO_BILL
        )
      )
    )

    on[PrestationStatusUpdated](_.id).update((state, event) =>
      state.copy(prestations =
        state.prestations.map(prestation =>
          if prestation.prestationId == event.prestationId then
            prestation.copy(status = event.status)
          else prestation
        )
      )
    )

    on[PrestationDeleted](_.id).update((state, event) =>
      state.copy(prestations =
        state.prestations.filterNot(_.prestationId == event.prestationId)
      )
    )

object AppointmentProjection:

  /** What the projection publishes after each commit, for the commands that
    * wait for it (`SyncCommandHandler`): the id of the event it processed and
    * the new states of the appointments it touched.
    */
  type Updates =
    Topic[IO, (UUID, Either[Throwable, Map[UUID, Option[AppointmentState]]])]

  def updates: IO[Updates] =
    Topic[IO, (UUID, Either[Throwable, Map[UUID, Option[AppointmentState]]])]
