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
          (_, therapistsPosition) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsPosition) <-
            getCompanyAppointments(companyId)
          now <- IO.realTimeInstant
        yield BillingDashboard(
          companyAppointments,
          now,
          oldest(therapistsPosition, appointmentsPosition)
        )

      override def getDirectoryDashboard(
          companyId: CompanyId
      ): IO[GetDirectoryDashboardOutput] =
        for
          (therapists, therapistsPosition) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsPosition) <-
            getCompanyAppointments(companyId)
          now <- IO.realTimeInstant
        yield DirectoryDashboard(
          therapists,
          companyAppointments,
          now,
          oldest(therapistsPosition, appointmentsPosition)
        )

      override def getActivityDashboard(
          companyId: CompanyId,
          unit: PeriodUnit,
          date: LocalDate
      ): IO[GetActivityDashboardOutput] =
        for
          (therapists, therapistsPosition) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsPosition) <-
            getCompanyAppointments(companyId)
        yield ActivityDashboard(
          therapists,
          companyAppointments,
          unit,
          date,
          oldest(therapistsPosition, appointmentsPosition)
        )

      // Each read has its own position, we only keep the oldest position between the two
      private def oldest(
          therapistsPosition: Long,
          appointmentsPosition: Long
      ): Long =
        appointmentsPosition.min(therapistsPosition)

      // The therapists of the company, with the position of the read
      private def getCompanyTherapists(
          companyId: CompanyId
      ): IO[(List[Therapist], Long)] =
        companyService.getCompany(companyId) *>
          therapistService.listTherapists().map { output =>
            (
              output.therapists.filter(_.companyId == companyId),
              output.globalPosition
            )
          }
      end getCompanyTherapists

      // The appointments of the company, with the position of the read
      private def getCompanyAppointments(
          companyId: CompanyId
      ): IO[(List[Appointment], Long)] =
        appointmentService
          .listCompanyAppointments(companyId)
          .map(output => (output.appointments, output.globalPosition))

    })
