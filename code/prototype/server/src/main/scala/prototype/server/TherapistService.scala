package prototype.server

import cats.effect.*
import cats.implicits.*
import prototype.api.*
import smithy4s.http4s.SimpleRestJsonBuilder

class TherapistServiceImpl(therapists: List[Therapist])
    extends TherapistService[IO]:
  override def listTherapists(): IO[ListTherapistsOutput] =
    ListTherapistsOutput(therapists).pure[IO]
  override def getTherapist(
      therapistId: TherapistId
  ): IO[GetTherapistOutput] =
    therapists.find(_.therapistId == therapistId) match
      case Some(therapist) => GetTherapistOutput(therapist).pure[IO]
      case None            => IO.raiseError(TherapistNotFound())
