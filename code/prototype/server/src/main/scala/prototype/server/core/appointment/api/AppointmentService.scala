package prototype.server.core.appointment.api

import java.util.UUID
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.commands.{
  CreateAppointment,
  CreatePrestation,
  DeleteAppointment,
  DeletePrestation,
  UpdateAppointment,
  UpdatePrestationStatus
}
import prototype.server.core.appointment.domain.AppointmentError
import prototype.server.core.appointment.domain.AppointmentError.*
import prototype.server.core.appointment.persistence.{
  AppointmentProjection,
  AppointmentRepository,
  AppointmentState
}

import persistent4s.{
  CommandRuntime,
  EventSourcedCommandHandler,
  SyncCommandHandler
}

import org.typelevel.log4cats.slf4j.Slf4jLogger
import smithy4s.time.Timestamp

/** The appointments, on Persistent4S. Commands go through the handlers; reads
  * come from the projection. The read model is updated a moment after a
  * command, so every change waits for the projection before answering
  * (`SyncCommandHandler`): the caller reads its own write (the client shows the
  * returned appointment, or reloads the list, right away).
  *
  * For now: later in the project the commands will not wait (asynchronous), and
  * the client will learn about the change from the events.
  *
  * @param appointments
  *   the read model, filled by the appointment projection
  * @param updates
  *   what the appointment projection publishes after each commit; it must be
  *   started with the same topic
  * @param therapists
  *   used to tell an unknown therapist from one without appointments
  */
class AppointmentServiceImpl(
    appointments: AppointmentRepository,
    updates: AppointmentProjection.Updates,
    therapists: TherapistService[IO]
)(using runtime: CommandRuntime[IO, PrototypeEvent])
    extends AppointmentService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  // The domain errors, as the API names them
  private val toApiError: AppointmentError => Throwable =
    case APPOINTMENT_NOT_FOUND(_)  => AppointmentNotFound()
    case INVALID_TIME_RANGE(_)     => InvalidTimeRange()
    case NO_THERAPIST(_)           => InvalidParticipants()
    case THERAPISTS_NOT_FOUND(ids) =>
      TherapistsNotFound(ids.map(TherapistId(_)))
    case PATIENTS_NOT_FOUND(ids)     => PatientsNotFound(ids.map(PatientId(_)))
    case THERAPIST_BUSY(ids)         => TherapistBusy(ids.map(TherapistId(_)))
    case PATIENT_BUSY(ids)           => PatientBusy(ids.map(PatientId(_)))
    case PRESTATION_NOT_FOUND(_, _)  => PrestationNotFound()
    case INVALID_PRESTATION(_)       => InvalidPrestation()
    case INVALID_STATUS_CHANGE(_, _) => InvalidStatusChange()
    // The ids are generated here, so a taken id cannot happen
    case error: APPOINTMENT_ALREADY_EXISTS => error
    case error: PRESTATION_ALREADY_EXISTS  => error

  // The type is stored as its name: a name nobody can read back means the
  // stored data is corrupted
  private def fromState(state: AppointmentState): IO[Appointment] =
    AppointmentType.values
      .find(_.stringValue == state.apptType)
      .liftTo[IO](
        IllegalStateException(
          s"Appointment ${state.appointmentId} has an unknown type: ${state.apptType}"
        )
      )
      .map(apptType =>
        Appointment(
          AppointmentId(state.appointmentId),
          state.therapistIds.map(TherapistId(_)),
          state.patientIds.map(PatientId(_)),
          apptType,
          TimeRange(
            Timestamp.fromEpochMilli(state.startTime),
            Timestamp.fromEpochMilli(state.endTime)
          ),
          state.prestations.map(prestation =>
            Prestation(
              AppointmentId(state.appointmentId),
              PrestationId(prestation.prestationId),
              prestation.label,
              prestation.quantity,
              prestation.unitPrice,
              prestation.status
            )
          )
        )
      )

  // How long a command waits for the projection before answering anyway
  private val syncTimeout = 5.seconds

  // The events the appointment projection reads: a command waits only for those
  private val projected = AppointmentProjection(appointments).filter

  private def waitingFor[C, S](
      handler: EventSourcedCommandHandler[
        C,
        S,
        PrototypeEvent,
        AppointmentError
      ]
  ): SyncCommandHandler[IO, C, PrototypeEvent, UUID, AppointmentState] =
    SyncCommandHandler.fromEventSourced(
      handler,
      runtime,
      toApiError,
      updates,
      projected,
      timeout = syncTimeout
    )

  // For now we use Sync handlers
  // NOTE: Change to asynchronous handlers later in the project, and let the client learn about the change from the events
  private val createAppointmentSync = waitingFor(CreateAppointment.Handler)
  private val updateAppointmentSync = waitingFor(UpdateAppointment.Handler)
  private val deleteAppointmentSync = waitingFor(DeleteAppointment.Handler)
  private val createPrestationSync = waitingFor(CreatePrestation.Handler)
  private val updatePrestationSync = waitingFor(UpdatePrestationStatus.Handler)
  private val deletePrestationSync = waitingFor(DeletePrestation.Handler)

  // Runs the command and returns the appointment as the projection saved it
  // (None once deleted). An update that changes nothing produces no event, and
  // a projection slower than the timeout does not undo a saved change: in both
  // cases the read model is read instead.
  private def runAndRead[C](
      sync: SyncCommandHandler[IO, C, PrototypeEvent, UUID, AppointmentState],
      command: C,
      id: UUID
  ): IO[Option[AppointmentState]] =
    sync
      .runSync(command)
      .flatMap(states => states.get(id).fold(appointments.find(id))(IO.pure))
      .recoverWith { case _: TimeoutException =>
        // The change is saved: only the read model is late. Say so in the
        // logs, since a projection this slow (or stuck) needs looking at
        logger.warn(
          s"Appointment $id: the change is saved, but the projection did not confirm it within $syncTimeout; answering from the read model"
        ) *> appointments.find(id)
      }

  // For a change after which the appointment must still exist
  private def existing(found: Option[AppointmentState]): IO[Appointment] =
    found.fold(IO.raiseError(AppointmentNotFound()))(fromState)

  // Fails with TherapistNotFound if the therapist does not exist
  override def listTherapistAppointments(
      therapistId: TherapistId
  ): IO[ListTherapistAppointmentsOutput] =
    therapists.getTherapist(therapistId) *>
      appointments
        .forTherapist(therapistId.value)
        .flatMap(_.traverse(fromState))
        .map(ListTherapistAppointmentsOutput.apply)

  override def getAppointment(
      appointmentId: AppointmentId
  ): IO[GetAppointmentOutput] =
    appointments.find(appointmentId.value).flatMap {
      case Some(state) => fromState(state).map(GetAppointmentOutput.apply)
      case None        => IO.raiseError(AppointmentNotFound())
    }

  // The id is generated here. If the projection is slow, the answer is built
  // from the request: a new appointment has no prestations yet.
  override def createAppointment(
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
      apptType: AppointmentType,
      timeRange: TimeRange
  ): IO[CreateAppointmentOutput] =
    for
      id <- IO.randomUUID
      found <- runAndRead(
        createAppointmentSync,
        CreateAppointment(
          id,
          therapistIds.map(_.value),
          patientIds.map(_.value),
          apptType.stringValue,
          timeRange
        ),
        id
      )
      _ <- logger.info(s"Created appointment $id")
      appointment <- found match
        case Some(state) => fromState(state)
        case None        =>
          IO.pure(
            Appointment(
              AppointmentId(id),
              therapistIds,
              patientIds,
              apptType,
              timeRange,
              Nil
            )
          )
    yield CreateAppointmentOutput(appointment)

  // The type, the participants and the time are all replaced
  override def updateAppointment(
      appointmentId: AppointmentId,
      apptType: AppointmentType,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
      timeRange: TimeRange
  ): IO[UpdateAppointmentOutput] =
    val id = appointmentId.value
    for
      found <- runAndRead(
        updateAppointmentSync,
        UpdateAppointment(
          id,
          therapistIds.map(_.value),
          patientIds.map(_.value),
          apptType.stringValue,
          timeRange
        ),
        id
      )
      _ <- logger.info(s"Updated appointment $id")
      appointment <- existing(found)
    yield UpdateAppointmentOutput(appointment)

  override def deleteAppointment(appointmentId: AppointmentId): IO[Unit] =
    val id = appointmentId.value
    for
      _ <- runAndRead(deleteAppointmentSync, DeleteAppointment(id), id)
      _ <- logger.info(s"Deleted appointment $id")
    yield ()

  // The prestation id is generated here
  override def createPrestation(
      appointmentId: AppointmentId,
      label: String,
      quantity: Int,
      unitPrice: Float
  ): IO[CreatePrestationOutput] =
    val id = appointmentId.value
    for
      prestationId <- IO.randomUUID
      found <- runAndRead(
        createPrestationSync,
        CreatePrestation(id, prestationId, label, unitPrice, quantity),
        id
      )
      _ <- logger.info(s"Added prestation $prestationId to appointment $id")
      appointment <- existing(found)
    yield CreatePrestationOutput(appointment)

  override def updatePrestationStatus(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      status: PrestationStatus
  ): IO[UpdatePrestationStatusOutput] =
    val id = appointmentId.value
    for
      found <- runAndRead(
        updatePrestationSync,
        UpdatePrestationStatus(id, prestationId.value, status),
        id
      )
      _ <- logger.info(
        s"Changed prestation ${prestationId.value} of appointment $id to ${status.stringValue}"
      )
      appointment <- existing(found)
    yield UpdatePrestationStatusOutput(appointment)

  override def deletePrestation(
      appointmentId: AppointmentId,
      prestationId: PrestationId
  ): IO[Unit] =
    val id = appointmentId.value
    for
      _ <- runAndRead(
        deletePrestationSync,
        DeletePrestation(id, prestationId.value),
        id
      )
      _ <- logger.info(
        s"Deleted prestation ${prestationId.value} of appointment $id"
      )
    yield ()
