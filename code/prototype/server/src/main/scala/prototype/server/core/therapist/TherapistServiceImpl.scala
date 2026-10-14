package prototype.server.core.therapist

import cats.effect.*
import prototype.api.*
import prototype.server.eventStore.*
import prototype.server.core.therapist.therapistModel.*

trait TherapistServiceImpl extends TherapistService[IO]:
  def getTherapists(therapistIds: List[TherapistId]): IO[List[Therapist]]

object TherapistServiceImpl:
  def apply(
      eventStore: EventStore,
      companyService: CompanyService[IO]
  ): IO[TherapistServiceImpl] =
    for
      view <- Ref.of[IO, Map[String, Therapist]](Map.empty)
      viewer = TherapistViewer(view)
      decider = TherapistDecider()
    yield new TherapistServiceImpl:

      val handler = new CommandHandler(decider, viewer, eventStore)

      override def listTherapists(): IO[ListTherapistsOutput] =
        viewer.getAllTherapists().map(ListTherapistsOutput.apply)

      override def getTherapist(
          therapistId: TherapistId
      ): IO[GetTherapistOutput] =
        viewer.getState(therapistId.value).flatMap {
          case Some(therapist) => IO.pure(GetTherapistOutput(therapist))
          case None            => IO.raiseError(TherapistNotFound(therapistId))
        }

      override def getTherapists(
          therapistIds: List[TherapistId]
      ): IO[List[Therapist]] =
        val wantedIds = therapistIds.toSet
        viewer.getAllTherapists().map { allTherapists =>
          allTherapists
            .filter(therapist => wantedIds.contains(therapist.therapistId))
        }

      // The therapist id (a trigram) is chosen by the caller, so it is also
      // the tag of the stream, and also how the seed creates therapists.
      override def createTherapist(
          firstName: String,
          lastName: String,
          therapistId: TherapistId,
          companyId: CompanyId,
          role: Role
      ): IO[CreateTherapistOutput] =
        val command = CreateTherapistCommand(
          therapistId,
          firstName,
          lastName,
          companyId,
          role
        )
        // Fails with CompanyNotFound if the company does not exist
        IO.print(s"Creating therapist $therapistId for company $companyId\n") *>
          companyService.getCompany(companyId).flatMap { _ =>
            handler.handle(therapistId.value, command).flatMap {
              case Right(therapist) =>
                IO.pure(CreateTherapistOutput(therapist.get))
              case Left(ALREADY_EXISTS) =>
                IO.raiseError(TherapistAlreadyExists())
            }
          }
