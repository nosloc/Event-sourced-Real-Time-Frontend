package prototype.server

import cats.effect.*
import cats.implicits.*
import org.http4s.HttpRoutes
import prototype.api.*
import smithy4s.http4s.SimpleRestJsonBuilder

object Routes:
  def all(
      staticRoutes: HttpRoutes[IO],
      companyServiceImpl: CompanyService[IO],
      therapistServiceImpl: TherapistService[IO],
      appointmentServiceImpl: AppointmentService[IO],
      authServiceImpl: AuthService[IO]
  ): Resource[IO, HttpRoutes[IO]] =
    for
      companies <- SimpleRestJsonBuilder
        .routes(
          companyServiceImpl
        )
        .resource
      therapists <- SimpleRestJsonBuilder
        .routes(
          therapistServiceImpl
        )
        .resource
      appointments <- SimpleRestJsonBuilder
        .routes(
          appointmentServiceImpl
        )
        .resource
      auth <- SimpleRestJsonBuilder
        .routes(
          authServiceImpl
        )
        .resource

    yield companies <+> therapists <+> appointments <+> staticRoutes <+> auth
