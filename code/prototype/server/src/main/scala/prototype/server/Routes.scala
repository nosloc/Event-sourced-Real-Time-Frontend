package prototype.server

import cats.effect.*
import cats.implicits.*
import org.http4s.HttpRoutes
import prototype.api.*
import smithy4s.http4s.SimpleRestJsonBuilder
import prototype.server.auth.AuthMiddleware

object Routes:
  def all(
      staticRoutes: HttpRoutes[IO],
      middleware: AuthMiddleware,
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
        .middleware(middleware)
        .resource
      therapists <- SimpleRestJsonBuilder
        .routes(
          therapistServiceImpl
        )
        .middleware(middleware)
        .resource
      appointments <- SimpleRestJsonBuilder
        .routes(
          appointmentServiceImpl
        )
        .middleware(middleware)
        .resource
      auth <- SimpleRestJsonBuilder
        .routes(
          authServiceImpl
        )
        .middleware(middleware)
        .resource

    yield companies <+> therapists <+> appointments <+> staticRoutes <+> auth
