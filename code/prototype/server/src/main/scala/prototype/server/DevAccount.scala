package prototype.server

import java.util.UUID

import scala.concurrent.duration.*

import cats.effect.IO

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.commands.CreateCompany
import prototype.server.core.company.domain.CompanyError
import prototype.server.core.company.domain.CompanyError.*
import prototype.server.core.therapist.commands.CreateTherapist
import prototype.server.core.therapist.domain.TherapistError
import prototype.server.core.therapist.domain.TherapistError.*

import persistent4s.CommandRuntime

/** The only data created at boot: the therapist `dev` (for developer) and its
  * company, so that someone can log in to an empty database. The demo data is
  * generated on request (`POST /api/demo-data`), never at boot.
  *
  * Both are created through their command handlers with fixed ids, so the event
  * store itself refuses a second one: a restart, even after a crash between the
  * two, never depends on how far the projections have caught up.
  */
object DevAccount:

  val therapistId: TherapistId = TherapistId("dev")
  val companyName: String = "Dev Company"

  // Fixed, so creating the company again is refused as "already exists"
  private val companyId: UUID = UUID.nameUUIDFromBytes(companyName.getBytes)

  private val companyAddress: Address =
    Address("Dev Street", "1", "Devtown", "VD")

  // Longer than the projections' leader lease (30 s): after a hard stop, the
  // next boot may wait that long before the projections run again
  private val visibleTimeout: FiniteDuration = 45.seconds

  /** Creates `dev` and its company unless they exist, then waits until `dev`
    * can log in (login reads the therapist projection).
    */
  def ensure(therapists: TherapistService[IO])(using
      CommandRuntime[IO, PrototypeEvent]
  ): IO[Unit] =
    for
      _ <- CreateCompany.Handler
        .runOrRaise[IO](CreateCompany(companyId, companyName, companyAddress)) {
          case error: CompanyError => error
        }
        .void
        .recover { case COMPANY_ALREADY_EXISTS(_) => () }
      _ <- CreateTherapist.Handler
        .runOrRaise[IO](
          CreateTherapist(
            therapistId.value,
            "Dev",
            "User",
            companyId,
            Role.PSYCHOLOGIST
          )
        ) { case error: TherapistError => error }
        .void
        .recover { case THERAPIST_ALREADY_EXISTS(_) => () }
      _ <- waitUntilVisible(therapists).timeoutTo(
        visibleTimeout,
        IO.raiseError(
          IllegalStateException(
            s"dev is still not readable after $visibleTimeout: are the projections running?"
          )
        )
      )
    yield ()

  // Only a missing therapist means "not there yet": any other error fails the
  // boot
  private def waitUntilVisible(therapists: TherapistService[IO]): IO[Unit] =
    therapists
      .getTherapist(therapistId)
      .as(true)
      .recover { case _: TherapistNotFound => false }
      .flatMap { visible =>
        if visible then IO.unit
        else IO.sleep(100.millis) *> waitUntilVisible(therapists)
      }
