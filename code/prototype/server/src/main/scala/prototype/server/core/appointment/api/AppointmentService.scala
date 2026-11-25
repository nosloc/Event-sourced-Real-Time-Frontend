package prototype.server.core.appointment.api

import java.util.UUID
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*

import prototype.api.*
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
  AppointmentRepository
}
import prototype.server.eventfeed.ApiCursor
import prototype.server.eventfeed.ApiCursor.toApi
import prototype.shared.appointment.AppointmentState
import prototype.shared.events.PrototypeEvent

import persistent4s.{
  CommandRuntime,
  EventEnvelope,
  EventSourcedCommandHandler,
  SyncCommandHandler
}

import org.typelevel.log4cats.slf4j.Slf4jLogger

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
  *   used to tell an unknown therapist from one without appointments, and to
  *   find the therapists of a company
  * @param companies
  *   used to tell an unknown company from one without appointments
  */
class AppointmentServiceImpl(
    appointments: AppointmentRepository,
    updates: AppointmentProjection.Updates,
    therapists: TherapistService[IO],
    companies: CompanyService[IO]
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

  private def fromState(state: AppointmentState): IO[Appointment] =
    IO.pure(AppointmentState.toAppointment(state))

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

  // `runSync` returns only the projection's states and drops the events the
  // command wrote. This copy of the handler also puts those events in
  // `written`, so their cursor can be read afterwards, even when the wait for
  // the projection times out (the events are written by then)
  private def keepingEvents[C](
      sync: SyncCommandHandler[IO, C, PrototypeEvent, UUID, AppointmentState],
      written: Ref[IO, List[EventEnvelope[PrototypeEvent]]]
  ): SyncCommandHandler[IO, C, PrototypeEvent, UUID, AppointmentState] =
    sync.copy(execute = command => sync.execute(command).flatTap(written.set))

  // Runs the command and returns the appointment as the projection saved it
  // (None once deleted), with the cursor of the last event it wrote (None when
  // it wrote none). An update that changes nothing produces no event, and a
  // projection slower than the timeout does not undo a saved change: in both
  // cases the read model is read instead.
  private def runAndRead[C](
      sync: SyncCommandHandler[IO, C, PrototypeEvent, UUID, AppointmentState],
      command: C,
      id: UUID
  ): IO[(Option[AppointmentState], Option[Cursor])] =
    for
      written <- IO.ref(List.empty[EventEnvelope[PrototypeEvent]])
      found <- keepingEvents(sync, written)
        .runSync(command)
        .flatMap(states => states.get(id).fold(appointments.find(id))(IO.pure))
        .recoverWith { case _: TimeoutException =>
          // The change is saved: only the read model is late. Say so in the
          // logs, since a projection this slow (or stuck) needs looking at
          logger.warn(
            s"Appointment $id: the change is saved, but the projection did not confirm it within $syncTimeout; answering from the read model"
          ) *> appointments.find(id)
        }
      events <- written.get
    yield (found, ApiCursor.lastWritten(events))

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
        .flatMap { case (states, cursor) =>
          states.traverse(fromState).map((_, cursor.toApi))
        }
        .map(ListTherapistAppointmentsOutput.apply)

  // Fails with CompanyNotFound if the company does not exist. The appointments
  // of all the company's therapists are read at once, so the cursor is the
  // one of that read
  override def listCompanyAppointments(
      companyId: CompanyId
  ): IO[ListCompanyAppointmentsOutput] =
    for
      _ <- companies.getCompany(companyId)
      therapistList <- therapists.listTherapists()
      companyTherapists = therapistList.therapists
        .filter(_.companyId == companyId)
        .map(_.therapistId.value)
      (companyAppointments, cursor) <- appointments.forTherapists(
        companyTherapists
      )
    yield ListCompanyAppointmentsOutput(
      companyAppointments.map(AppointmentState.toAppointment),
      cursor.toApi
    )

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
      (found, written) <- runAndRead(
        createAppointmentSync,
        CreateAppointment(
          id,
          therapistIds.map(_.value),
          patientIds.map(_.value),
          apptType,
          timeRange
        ),
        id
      )
      _ <- logger.info(s"Created appointment $id")
      cursor <- ApiCursor.required(written)
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
    yield CreateAppointmentOutput(appointment, cursor)

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
      (found, cursor) <- runAndRead(
        updateAppointmentSync,
        UpdateAppointment(
          id,
          therapistIds.map(_.value),
          patientIds.map(_.value),
          apptType,
          timeRange
        ),
        id
      )
      _ <- logger.info(s"Updated appointment $id")
      appointment <- existing(found)
    yield UpdateAppointmentOutput(appointment, cursor)

  override def deleteAppointment(
      appointmentId: AppointmentId
  ): IO[DeleteAppointmentOutput] =
    val id = appointmentId.value
    for
      (_, written) <- runAndRead(deleteAppointmentSync, DeleteAppointment(id), id)
      _ <- logger.info(s"Deleted appointment $id")
      cursor <- ApiCursor.required(written)
    yield DeleteAppointmentOutput(cursor)

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
      (found, written) <- runAndRead(
        createPrestationSync,
        CreatePrestation(id, prestationId, label, unitPrice, quantity),
        id
      )
      _ <- logger.info(s"Added prestation $prestationId to appointment $id")
      appointment <- existing(found)
      cursor <- ApiCursor.required(written)
    yield CreatePrestationOutput(appointment, cursor)

  override def updatePrestationStatus(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      status: PrestationStatus
  ): IO[UpdatePrestationStatusOutput] =
    val id = appointmentId.value
    for
      (found, written) <- runAndRead(
        updatePrestationSync,
        UpdatePrestationStatus(id, prestationId.value, status),
        id
      )
      _ <- logger.info(
        s"Changed prestation ${prestationId.value} of appointment $id to ${status.stringValue}"
      )
      appointment <- existing(found)
      cursor <- ApiCursor.required(written)
    yield UpdatePrestationStatusOutput(appointment, cursor)

  override def deletePrestation(
      appointmentId: AppointmentId,
      prestationId: PrestationId
  ): IO[DeletePrestationOutput] =
    val id = appointmentId.value
    for
      (_, written) <- runAndRead(
        deletePrestationSync,
        DeletePrestation(id, prestationId.value),
        id
      )
      _ <- logger.info(
        s"Deleted prestation ${prestationId.value} of appointment $id"
      )
      cursor <- ApiCursor.required(written)
    yield DeletePrestationOutput(cursor)
