package prototype.server

import cats.effect.*
import prototype.api.*
import smithy4s.http4s.SimpleRestJsonBuilder

class TherapistServiceImpl(therapists: List[Therapist])
    extends TherapistService[IO]:
  override def getTherapists(): IO[GetTherapistsOutput] =
    IO.pure(GetTherapistsOutput(therapists))

object TherapistServiceImpl:
  val lestoises = Company("Les Toises", "Av. des Mousquines 4, 1005 Lausanne")
  val chuv = Company("CHUV", "Rue du Bugnon 46, 1005 Lausanne")
  val therapists = List(
    Therapist("Alexandre", "Robert", "ARB", lestoises),
    Therapist("Marie", "Dupont", "MDP", chuv),
    Therapist("Jean", "Martin", "JMA", lestoises),
    Therapist("Sophie", "Durand", "SDA", chuv)
  )
  def routes() =
    SimpleRestJsonBuilder
      .routes(
        TherapistServiceImpl(therapists)
      )
      .resource
