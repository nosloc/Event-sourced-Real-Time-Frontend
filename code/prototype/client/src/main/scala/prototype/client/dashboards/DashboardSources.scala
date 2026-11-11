package prototype.client.dashboards

import java.util.UUID

import prototype.api.{Appointment, CompanyId, Therapist}
import prototype.client.appointments.LiveAppointmentsForCompany
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.therapists.LiveTherapistsForCompany

import com.raquo.laminar.api.L.*

/** What the dashboards of one company are computed from in the browser: its
  * therapists and its appointments, live. It is one live value of both (read
  * whole by the directory and activity dashboards), and keeps the two apart so
  * that a dashboard can read only one (billing reads only the appointments).
  */
final case class DashboardSources(
    therapists: LiveData[List[Therapist]],
    appointments: LiveData[List[Appointment]]
) extends LiveData[(List[Therapist], List[Appointment])]:
  private val combined: LiveData[(List[Therapist], List[Appointment])] =
    therapists.combineWith(appointments)

  val valueS: Signal[Option[(List[Therapist], List[Appointment])]] =
    combined.valueS
  val loadingS: Signal[Boolean] = combined.loadingS
  val errorS: Signal[Option[Throwable]] = combined.errorS

  /** Mount this on an element that stays on screen: it keeps the sources
    * running while `activeS` is true, and lets them stop otherwise (their state
    * is dropped, and they load again once it is true).
    */
  def keepLoadedWhile(activeS: Signal[Boolean]): Mod[HtmlElement] =
    activeS.distinct.flatMapSwitch(
      if _ then valueS else Val(None)
    ) --> Observer.empty

object DashboardSources:
  def from(
      feed: LiveFeed,
      allTherapists: LiveData[List[Therapist]],
      companyIdS: Signal[UUID]
  ): DashboardSources =
    val apiCompanyIdS = companyIdS.map(CompanyId.apply)

    val companyTherapists =
      LiveTherapistsForCompany(
        allTherapists,
        apiCompanyIdS
      )

    val companyAppointments =
      LiveAppointmentsForCompany(
        feed,
        apiCompanyIdS,
        companyTherapists
      )

    DashboardSources(
      companyTherapists,
      companyAppointments
    )
