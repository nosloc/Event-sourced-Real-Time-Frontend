package prototype.server.core.dashboards

import cats.effect.*

import prototype.api.*
import prototype.shared.Cursors
import prototype.shared.dashboards.*

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
          (_, therapistsCursor) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsCursor) <-
            getCompanyAppointments(companyId)
          now <- IO.realTimeInstant
          figures = BillingDashboard(
            companyAppointments,
            now
          )
        yield GetBillingDashboardOutput(
          figures,
          oldest(therapistsCursor, appointmentsCursor)
        )

      override def getDirectoryDashboard(
          companyId: CompanyId
      ): IO[GetDirectoryDashboardOutput] =
        for
          (therapists, therapistsCursor) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsCursor) <-
            getCompanyAppointments(companyId)
          now <- IO.realTimeInstant
          figures = DirectoryDashboard(
            therapists,
            companyAppointments,
            now
          )
        yield GetDirectoryDashboardOutput(
          figures,
          oldest(therapistsCursor, appointmentsCursor)
        )

      override def getActivityDashboard(
          companyId: CompanyId,
          unit: PeriodUnit,
          date: LocalDate
      ): IO[GetActivityDashboardOutput] =
        for
          (therapists, therapistsCursor) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsCursor) <-
            getCompanyAppointments(companyId)
          figures = ActivityDashboard(
            therapists,
            companyAppointments,
            unit,
            date
          )
        yield GetActivityDashboardOutput(
          figures,
          oldest(therapistsCursor, appointmentsCursor)
        )

      // Each read has its own cursor, we only keep the oldest of the two
      private def oldest(
          therapistsCursor: Cursor,
          appointmentsCursor: Cursor
      ): Cursor =
        Cursors.ordering.min(therapistsCursor, appointmentsCursor)

      // The therapists of the company, with the cursor of the read
      private def getCompanyTherapists(
          companyId: CompanyId
      ): IO[(List[Therapist], Cursor)] =
        companyService.getCompany(companyId) *>
          therapistService.listTherapists().map { output =>
            (
              output.therapists.filter(_.companyId == companyId),
              output.cursor
            )
          }
      end getCompanyTherapists

      // The appointments of the company, with the cursor of the read
      private def getCompanyAppointments(
          companyId: CompanyId
      ): IO[(List[Appointment], Cursor)] =
        appointmentService
          .listCompanyAppointments(companyId)
          .map(output => (output.appointments, output.cursor))

    })
