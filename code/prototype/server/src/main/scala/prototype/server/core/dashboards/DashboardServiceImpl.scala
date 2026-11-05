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
          (therapists, _) <- getCompanyTherapists(companyId)
          (companyAppointments, _) <- getCompanyAppointments(therapists)
          now <- IO.realTimeInstant
        yield BillingDashboard(companyAppointments, now)

      override def getDirectoryDashboard(
          companyId: CompanyId
      ): IO[GetDirectoryDashboardOutput] =
        for
          (therapists, _) <- getCompanyTherapists(companyId)
          (companyAppointments, _) <- getCompanyAppointments(therapists)
          now <- IO.realTimeInstant
        yield DirectoryDashboard(therapists, companyAppointments, now)

      override def getActivityDashboard(
          companyId: CompanyId,
          unit: PeriodUnit,
          date: LocalDate
      ): IO[GetActivityDashboardOutput] =
        for
          (therapists, therapistsPosition) <- getCompanyTherapists(companyId)
          (companyAppointments, appointmentsPosition) <-
            getCompanyAppointments(therapists)
        yield ActivityDashboard(
          therapists,
          companyAppointments,
          unit,
          date,
          // Each read has its own position: the dashboard is only as recent as
          // the oldest of them
          (therapistsPosition :: appointmentsPosition.toList).min
        )

      // The therapists of the company, with the position of the read
      private def getCompanyTherapists(
          companyId: CompanyId
      ): IO[(List[Therapist], Long)] =
        // Fails with CompanyNotFound if the company does not exist
        companyService.getCompany(companyId) *>
          therapistService.listTherapists().map { output =>
            (
              output.therapists.filter(_.companyId == companyId),
              output.globalPosition
            )
          }
      end getCompanyTherapists
      // The appointments of the company, read one therapist at a time, with
      // the oldest position of those reads (None: no therapist, no read)
      private def getCompanyAppointments(
          companyTherapists: List[Therapist]
      ): IO[(List[Appointment], Option[Long])] =
        companyTherapists
          .map(_.therapistId)
          .traverse(
            appointmentService.listTherapistAppointments(_)
          )
          .map(outputs =>
            (
              // An appointment with two therapists of the company is listed
              // once per therapist, so it is kept only once
              outputs.flatMap(_.appointments).distinctBy(_.appointmentId),
              outputs.map(_.globalPosition).minOption
            )
          )
      end getCompanyAppointments

    })
