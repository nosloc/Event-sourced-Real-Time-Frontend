package prototype.server

import cats.effect.*
import cats.implicits.*

import prototype.api.*
import prototype.server.auth.AuthMiddleware

import org.http4s.HttpRoutes
import smithy4s.http4s.SimpleRestJsonBuilder

object Routes:
  def all(
      staticRoutes: HttpRoutes[IO],
      middleware: AuthMiddleware,
      companyServiceImpl: CompanyService[IO],
      therapistServiceImpl: TherapistService[IO],
      appointmentServiceImpl: AppointmentService[IO],
      patientServiceImpl: PatientService[IO],
      dashboardServiceImpl: DashboardService[IO],
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
      patients <- SimpleRestJsonBuilder
        .routes(
          patientServiceImpl
        )
        .middleware(middleware)
        .resource
      dashboards <- SimpleRestJsonBuilder
        .routes(
          dashboardServiceImpl
        )
        .middleware(middleware)
        .resource
      auth <- SimpleRestJsonBuilder
        .routes(
          authServiceImpl
        )
        .middleware(middleware)
        .resource

    yield companies <+> therapists <+> appointments <+> staticRoutes <+> auth <+> patients <+> dashboards
