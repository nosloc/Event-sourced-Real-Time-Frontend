package prototype.server.demo

import cats.effect.{IO, Ref}
import cats.effect.std.Random

import prototype.api.*

import org.typelevel.log4cats.slf4j.Slf4jLogger

/** `POST /api/demo-data`: generates the demo data once, on a database that only
  * holds the dev account.
  *
  * @param running
  *   set while a generation runs, and left set after one succeeded: a second
  *   call is refused at once instead of waiting for the therapist projection
  */
final class DemoDataServiceImpl private (
    generator: DemoDataGenerator,
    therapists: TherapistService[IO],
    running: Ref[IO, Boolean]
) extends DemoDataService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  // One generation at a time: the flag is taken for the whole run and
  // released however it ends (success, failure or cancellation), so the call
  // can be made again. A refused call never touches it.
  // Uncancelable between taking the flag and installing its release, so a
  // cancellation cannot leave the flag taken
  override def generateDemoData(): IO[GenerateDemoDataOutput] =
    IO.uncancelable { poll =>
      running.getAndSet(true).flatMap { alreadyRunning =>
        if alreadyRunning then IO.raiseError(DemoDataBeingGenerated())
        else poll(generate).guarantee(running.set(false))
      }
    }

  // Adds a new world beside the data already there: the existing therapists
  // are read only so the planner draws other trigrams
  private def generate: IO[GenerateDemoDataOutput] =
    for
      existing <- therapists
        .listTherapists()
        .map(_.therapists.map(_.therapistId))
      start <- IO.monotonic
      _ <- logger.info("Demo data: building the plan")
      today <- IO.realTimeInstant.map(_.atZone(DayPlanner.zone).toLocalDate)
      random <- Random.scalaUtilRandom[IO]
      plan <- DemoPlan.build(random, today, existing.map(_.value).toSet)
      _ <- logger.info("Demo data: writing the plan")
      counts <- generator.run(plan)
      end <- IO.monotonic
      output = toOutput(counts, (end - start).toMillis)
      _ <- logger.info(s"Demo data: done in ${output.durationMillis} ms")
    yield output

  private def toOutput(
      counts: DemoDataCounts,
      durationMillis: Long
  ): GenerateDemoDataOutput =
    GenerateDemoDataOutput(
      companies = counts.companies,
      therapists = counts.therapists,
      therapistsWithAppointments = counts.therapistsWithAppointments,
      patients = counts.patients,
      appointmentsByType = AppointmentType.values.map(apptType =>
        AppointmentTypeCount(
          apptType,
          counts.appointmentsByType.getOrElse(apptType, 0)
        )
      ),
      prestationsByStatus = PrestationStatus.values.map(status =>
        PrestationStatusCount(
          status,
          counts.prestationsByStatus.getOrElse(status, 0)
        )
      ),
      skippedWrites = counts.skippedWrites,
      durationMillis = durationMillis
    )

object DemoDataServiceImpl:

  def make(
      generator: DemoDataGenerator,
      therapists: TherapistService[IO]
  ): IO[DemoDataService[IO]] =
    Ref
      .of[IO, Boolean](false)
      .map(new DemoDataServiceImpl(generator, therapists, _))
