package prototype.client.dashboards

import java.time.Instant
import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData}
import prototype.shared.dashboards.{
  DirectoryDashboard,
  DirectoryDashboardEvents
}

import com.raquo.laminar.api.L.*

/** The directory dashboard of one company, either reloaded from the server or
  * computed in the browser.
  */
object LiveDirectoryDashboard:

  def apply(
      feed: LiveFeed,
      companyIdS: Signal[UUID]
  ): LiveData[DirectoryFigures] =
    new RefreshedLiveData[UUID, DirectoryFigures](
      feed = feed,
      key = companyIdS,
      load = companyId =>
        Api.dashboards
          .getDirectoryDashboard(CompanyId(companyId))
          .map(response => (response.dashboard, response.globalPosition)),
      triggerRefresh = message =>
        DirectoryDashboardEvents.types.contains(message.eventType)
    )

  /** Computed in the browser from the company's live therapists and
    * appointments, with the shared `DirectoryDashboard`: no request when an
    * event arrives.
    */
  def replayed(
      companyTherapists: LiveData[List[Therapist]],
      companyAppointments: LiveData[List[Appointment]]
  ): LiveData[DirectoryFigures] =
    companyTherapists
      .combineWith(companyAppointments)
      .map((therapists, appointments) =>
        DirectoryDashboard(
          therapists,
          appointments,
          Instant.now()
        )
      )
end LiveDirectoryDashboard
