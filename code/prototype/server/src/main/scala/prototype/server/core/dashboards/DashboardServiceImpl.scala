package prototype.server.core.dashboards

import cats.effect.*
import cats.implicits.*

import prototype.api.*

import smithy4s.time.LocalDate

trait DashboardServiceImpl extends DashboardService[IO]

object DashboardServiceImpl:
  def apply(
      companyService: CompanyService[IO],
      therapistService: TherapistService[IO],
      appointmentService: AppointmentService[IO]
  ): IO[DashboardServiceImpl] =
    IO.pure(new DashboardServiceImpl {

      override def getBillingDashboard(
          companyId: CompanyId
      ): IO[GetBillingDashboardOutput] =
        for
          therapists <- getCompanyTherapists(companyId)
          companyAppointments <- getCompanyAppointments(therapists)
          now <- IO.realTimeInstant
        yield BillingDashboard(companyAppointments, now)

      override def getDirectoryDashboard(
          companyId: CompanyId
      ): IO[GetDirectoryDashboardOutput] =
        for
          therapists <- getCompanyTherapists(companyId)
          companyAppointments <- getCompanyAppointments(therapists)
          now <- IO.realTimeInstant
        yield DirectoryDashboard(therapists, companyAppointments, now)

      override def getActivityDashboard(
          companyId: CompanyId,
          unit: PeriodUnit,
          date: LocalDate
      ): IO[GetActivityDashboardOutput] =
        for
          therapists <- getCompanyTherapists(companyId)
          companyAppointments <- getCompanyAppointments(therapists)
        yield ActivityDashboard(therapists, companyAppointments, unit, date)

      private def getCompanyTherapists(
          companyId: CompanyId
      ): IO[List[Therapist]] =
        // Fails with CompanyNotFound if the company does not exist
        companyService.getCompany(companyId) *>
          therapistService.listTherapists().map { therapists =>
            therapists.therapists.filter(_.companyId == companyId)
          }
      end getCompanyTherapists
      private def getCompanyAppointments(
          companyTherapists: List[Therapist]
      ): IO[List[Appointment]] =
        companyTherapists
          .map(_.therapistId)
          .traverse(
            appointmentService.listTherapistAppointments(_)
          )
          .map(_.flatMap(_.appointments))
          // An appointment with two therapists of the company is listed once
          // per therapist, so it is kept only once
          .map(_.distinctBy(_.appointmentId))
      end getCompanyAppointments

    })
