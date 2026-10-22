package prototype.server.core.therapist.api

import cats.effect.IO
import cats.syntax.all.*

import prototype.api.*
import prototype.server.core.PrototypeEvent
import prototype.server.core.therapist.commands.CreateTherapist
import prototype.server.core.therapist.domain.TherapistError
import prototype.server.core.therapist.domain.TherapistError.*
import prototype.server.core.therapist.persistence.{
  TherapistRepository,
  TherapistState
}

import persistent4s.CommandRuntime

import org.typelevel.log4cats.slf4j.Slf4jLogger

class TherapistServiceImpl(
    therapists: TherapistRepository
)(using CommandRuntime[IO, PrototypeEvent])
    extends TherapistService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  // The role is stored as its name: a name nobody can read back means the
  // stored data is corrupted, not that the caller did something wrong
  private def fromState(state: TherapistState): IO[Therapist] =
    Role.values
      .find(_.stringValue == state.role)
      .liftTo[IO](
        IllegalStateException(
          s"Therapist ${state.therapistId} has an unknown role: ${state.role}"
        )
      )
      .map(role =>
        Therapist(
          state.firstName,
          state.lastName,
          TherapistId(state.therapistId),
          CompanyId(state.companyId),
          role
        )
      )

  // Sorted by id: the list is shown to users and a projection has no stable order
  override def listTherapists(): IO[ListTherapistsOutput] =
    therapists.all
      .flatMap(_.sortBy(_.therapistId).traverse(fromState))
      .map(ListTherapistsOutput.apply)

  override def getTherapist(therapistId: TherapistId): IO[GetTherapistOutput] =
    therapists.find(therapistId.value).flatMap {
      case Some(state) => fromState(state).map(GetTherapistOutput.apply)
      case None        => IO.raiseError(TherapistNotFound(therapistId))
    }

  // The therapist id (a trigram) is chosen by the caller. The handler also
  // checks that the company exists, by reading the company's own events.
  override def createTherapist(
      firstName: String,
      lastName: String,
      therapistId: TherapistId,
      companyId: CompanyId,
      role: Role
  ): IO[CreateTherapistOutput] =
    for
      _ <- CreateTherapist.Handler.runOrRaise[IO](
        CreateTherapist(
          therapistId.value,
          firstName,
          lastName,
          companyId.value,
          role
        )
      ) {
        case THERAPIST_ALREADY_EXISTS(_)    => TherapistAlreadyExists()
        case THERAPIST_COMPANY_NOT_FOUND(_) => CompanyNotFound()
        case other: TherapistError          => other
      }
      _ <- logger.info(
        s"Created therapist ${therapistId.value} for company ${companyId.value}"
      )
    yield CreateTherapistOutput(
      Therapist(firstName, lastName, therapistId, companyId, role)
    )
