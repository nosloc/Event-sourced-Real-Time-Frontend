package prototype.server

import cats.effect.*
import prototype.api.*
import smithy4s.http4s.SimpleRestJsonBuilder

class TherapistServiceImpl(therapists: List[Therapist])
    extends TherapistService[IO]:
  override def getTherapists(): IO[GetTherapistsOutput] =
    IO.pure(GetTherapistsOutput(therapists))

object TherapistServiceImpl:
  val clinicA = Company("Example Clinic", "1 Example Street, 1000 Exampleville")
  val clinicB = Company("Demo Health Center", "2 Sample Avenue, 2000 Sampletown")
  val therapists = List(
    Therapist("Alexandre", "Robert", "ARB", clinicA),
    Therapist("Marie", "Dupont", "MDP", clinicB),
    Therapist("Jean", "Martin", "JMA", clinicA),
    Therapist("Sophie", "Durand", "SDA", clinicB)
  )
  def routes() =
    SimpleRestJsonBuilder
      .routes(
        TherapistServiceImpl(therapists)
      )
      .resource
