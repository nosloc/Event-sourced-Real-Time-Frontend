package prototype.server.demo

import java.util.UUID

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.effect.syntax.all.*
import cats.syntax.all.*

import prototype.api.{AppointmentType, PrestationStatus, TimeRange}
import prototype.server.core.PrototypeEvent
import prototype.server.core.appointment.commands.{
  CreateAppointment,
  CreatePrestation,
  UpdatePrestationStatus
}
import prototype.server.core.appointment.persistence.AppointmentRepository
import prototype.server.core.company.commands.CreateCompany
import prototype.server.core.company.persistence.CompanyRepository
import prototype.server.core.patient.commands.CreatePatient
import prototype.server.core.patient.persistence.PatientRepository
import prototype.server.core.therapist.commands.CreateTherapist
import prototype.server.core.therapist.persistence.TherapistRepository

import persistent4s.CommandRuntime

import org.typelevel.log4cats.slf4j.Slf4jLogger
import smithy4s.time.Timestamp

/** What one generation wrote successfully (not what was planned). */
final case class DemoDataCounts(
    companies: Int,
    therapists: Int,
    therapistsWithAppointments: Int,
    patients: Int,
    appointmentsByType: Map[AppointmentType, Int],
    prestationsByStatus: Map[PrestationStatus, Int],
    skippedWrites: Int
)

/** Writes a `DemoPlan` through the command handlers, with the ids the plan
  * chose. The handlers check every rule against the event streams, so nothing
  * waits for a projection between two writes; the read tables are waited for
  * once, at the end, so the dashboards are complete when `run` returns.
  *
  * A write that a handler refuses (a rule it breaks) is logged, counted as
  * skipped, and the writes that depend on it are skipped (and counted) too. Any
  * other failure (the database, a bug) stops the generation and fails `run`.
  *
  * The four repositories are only read, to wait until the read tables show what
  * was written.
  */
class DemoDataGenerator(
    companies: CompanyRepository,
    therapists: TherapistRepository,
    patients: PatientRepository,
    appointments: AppointmentRepository
)(using CommandRuntime[IO, PrototypeEvent]):

  private val logger = Slf4jLogger.getLogger[IO]

  // How long the read tables may take to show everything before `run` answers
  // anyway
  private val catchUpTimeout = 30.seconds

  // An appointment as written: its prestations with the status they reached
  private final case class WrittenAppointment(
      id: UUID,
      apptType: AppointmentType,
      prestations: List[(UUID, PrestationStatus)]
  )

  private final case class WrittenTherapist(
      trigram: String,
      patients: List[UUID],
      appointments: List[WrittenAppointment]
  )

  def run(plan: DemoPlan): IO[DemoDataCounts] =
    for
      skipped <- Ref.of[IO, Int](0)
      writtenCompanies <- plan.companies.filterA(writeCompany(_, skipped))
      staff <- writtenCompanies
        .flatMap(company => company.therapists.map(company.id -> _))
        .filterA((companyId, therapist) =>
          writeTherapist(companyId, therapist, skipped)
        )
        .map(_.map(_._2))
      // One at a time: every patient event carries the same global tag, so
      // concurrent creations conflict
      writtenPatients <- staff.traverse(therapist =>
        therapist.patients.filterA(writePatient(_, skipped)).map(_.map(_.id))
      )
      written <- staff
        .zip(writtenPatients)
        .parTraverseN(DemoDataSizes.ParallelTherapists) { (therapist, ids) =>
          therapist.appointments
            .traverse(writeAppointment(therapist, _, skipped))
            .map(found =>
              WrittenTherapist(therapist.trigram, ids, found.flatten)
            )
        }
      _ <- waitUntilCaughtUp(writtenCompanies.map(_.id), written)
      skippedWrites <- skipped.get
    yield DemoDataCounts(
      companies = writtenCompanies.size,
      therapists = written.size,
      therapistsWithAppointments = written.count(_.appointments.nonEmpty),
      patients = written.map(_.patients.size).sum,
      appointmentsByType = written
        .flatMap(_.appointments)
        .groupMapReduce(_.apptType)(_ => 1)(_ + _),
      prestationsByStatus = written
        .flatMap(_.appointments.flatMap(_.prestations))
        .groupMapReduce(_._2)(_ => 1)(_ + _),
      skippedWrites = skippedWrites
    )

  // Runs one write (a handler's `run`): a refusal is logged and answers false;
  // a failed effect is not a refusal and fails the generation
  private def attempt[R <: Throwable](what: String)(
      write: IO[Either[R, ?]]
  ): IO[Boolean] =
    write.flatMap {
      case Right(_)        => IO.pure(true)
      case Left(rejection) =>
        logger
          .warn(
            s"Demo data: $what was refused and skipped: ${rejection.getMessage}"
          )
          .as(false)
    }

  // Counts a refused write and the planned writes that depended on it
  private def skip(skipped: Ref[IO, Int], writes: Int): IO[Unit] =
    skipped.update(_ + writes)

  private def writesOf(prestation: PlannedPrestation): Int =
    1 + prestation.steps.size

  private def writesOf(appointment: PlannedAppointment): Int =
    1 + appointment.prestations.map(writesOf).sum

  private def writesOf(therapist: PlannedTherapist): Int =
    1 + therapist.patients.size + therapist.appointments.map(writesOf).sum

  private def writeCompany(
      company: PlannedCompany,
      skipped: Ref[IO, Int]
  ): IO[Boolean] =
    attempt(s"company ${company.id}")(
      CreateCompany.Handler
        .run[IO](CreateCompany(company.id, company.name, company.address))
    ).flatTap(ok =>
      skip(skipped, 1 + company.therapists.map(writesOf).sum).unlessA(ok)
    )

  // The handler checks the company in the company's own events: no need to
  // wait for the company projection
  private def writeTherapist(
      companyId: UUID,
      therapist: PlannedTherapist,
      skipped: Ref[IO, Int]
  ): IO[Boolean] =
    attempt(s"therapist ${therapist.trigram}")(
      CreateTherapist.Handler
        .run[IO](
          CreateTherapist(
            therapist.trigram,
            therapist.firstName,
            therapist.lastName,
            companyId,
            therapist.role
          )
        )
    ).flatTap(ok => skip(skipped, writesOf(therapist)).unlessA(ok))

  // An appointment naming a refused patient is refused on its own (and
  // counted then)
  private def writePatient(
      patient: PlannedPatient,
      skipped: Ref[IO, Int]
  ): IO[Boolean] =
    attempt(s"patient ${patient.id}")(
      CreatePatient.Handler
        .run[IO](
          CreatePatient(
            patient.id,
            patient.firstName,
            patient.lastName,
            patient.birthDate
          )
        )
    ).flatTap(ok => skip(skipped, 1).unlessA(ok))

  private def writeAppointment(
      therapist: PlannedTherapist,
      appointment: PlannedAppointment,
      skipped: Ref[IO, Int]
  ): IO[Option[WrittenAppointment]] =
    // Built inside IO: a wrong patient index (a planner bug) fails the
    // generation instead of throwing while the writes are being assembled
    val command = IO(
      CreateAppointment(
        appointment.id,
        List(therapist.trigram),
        appointment.patients.map(therapist.patients(_).id),
        appointment.apptType.stringValue,
        TimeRange(
          Timestamp.fromEpochMilli(appointment.start.toEpochMilli),
          Timestamp.fromEpochMilli(appointment.end.toEpochMilli)
        )
      )
    )
    attempt(s"appointment ${appointment.id}")(
      command.flatMap(CreateAppointment.Handler.run[IO](_))
    ).flatMap { ok =>
      if ok then
        appointment.prestations
          .traverse(writePrestation(appointment.id, _, skipped))
          .map(found =>
            Some(
              WrittenAppointment(
                appointment.id,
                appointment.apptType,
                found.flatten
              )
            )
          )
      else skip(skipped, writesOf(appointment)).as(None)
    }

  // Creates the prestation, then makes its status changes in order; a refused
  // change stops the ones after it. Answers the status it reached.
  private def writePrestation(
      appointmentId: UUID,
      prestation: PlannedPrestation,
      skipped: Ref[IO, Int]
  ): IO[Option[(UUID, PrestationStatus)]] =
    attempt(s"prestation ${prestation.id}")(
      CreatePrestation.Handler
        .run[IO](
          CreatePrestation(
            appointmentId,
            prestation.id,
            prestation.label,
            prestation.unitPrice,
            1
          )
        )
    ).flatMap { ok =>
      if ok then
        writeSteps(appointmentId, prestation.id, prestation.steps, skipped)
          .map(status => Some(prestation.id -> status))
      else skip(skipped, writesOf(prestation)).as(None)
    }

  private def writeSteps(
      appointmentId: UUID,
      prestationId: UUID,
      steps: List[PrestationStatus],
      skipped: Ref[IO, Int]
  ): IO[PrestationStatus] =
    steps
      .foldLeftM((PrestationStatus.TO_BILL: PrestationStatus, true)) {
        case ((status, true), next) =>
          attempt(s"status $next of prestation $prestationId")(
            UpdatePrestationStatus.Handler
              .run[IO](
                UpdatePrestationStatus(appointmentId, prestationId, next)
              )
          ).flatMap(ok =>
            if ok then IO.pure((next, true))
            else skip(skipped, 1).as((status, false))
          )
        case ((status, false), _) => skip(skipped, 1).as((status, false))
      }
      .map(_._1)

  // The projections fill the read tables a moment after the writes: wait
  // until they show everything, so the dashboards are complete on return
  private def waitUntilCaughtUp(
      companyIds: List[UUID],
      written: List[WrittenTherapist]
  ): IO[Unit] =
    def caughtUp: IO[Boolean] =
      for
        companiesShown <- companies.all.map(_.map(_.companyId).toSet)
        therapistsShown <- therapists.all.map(_.map(_.therapistId).toSet)
        patientsShown <- patients.all.map(_.map(_.patientId).toSet)
        appointmentsShown <- written
          .filter(_.appointments.nonEmpty)
          .forallM(therapist =>
            appointments.forTherapist(therapist.trigram).map { (states, _) =>
              val statuses = states
                .map(state =>
                  state.appointmentId ->
                    state.prestations.map(p => p.prestationId -> p.status).toMap
                )
                .toMap
              therapist.appointments.forall(appointment =>
                statuses
                  .get(appointment.id)
                  .exists(shown =>
                    appointment.prestations
                      .forall((id, status) => shown.get(id).contains(status))
                  )
              )
            }
          )
      yield companyIds.forall(companiesShown.contains) &&
        written.forall(t => therapistsShown.contains(t.trigram)) &&
        written.forall(_.patients.forall(patientsShown.contains)) &&
        appointmentsShown

    // A failed read is not a reason to fail a generation already written
    def poll: IO[Unit] =
      caughtUp
        .handleErrorWith(error =>
          logger
            .warn(s"Demo data: reading the tables failed, retrying: $error")
            .as(false)
        )
        .flatMap(done => if done then IO.unit else IO.sleep(200.millis) *> poll)

    poll.timeoutTo(
      catchUpTimeout,
      logger.warn(
        s"Demo data: the read tables did not show everything after $catchUpTimeout; answering anyway"
      )
    )
