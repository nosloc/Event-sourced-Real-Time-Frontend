package prototype.client.dashboards

import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData}
import prototype.shared.dashboards.DirectoryDashboardEvents

import com.raquo.laminar.api.L.*

/** The directory dashboard of one company, reloaded from the server when one of
  * its events arrives.
  */
object LiveDirectoryDashboard:

  def apply(
      feed: LiveFeed,
      companyIdS: Signal[UUID]
  ): LiveData[GetDirectoryDashboardOutput] =
    new RefreshedLiveData[UUID, GetDirectoryDashboardOutput](
      feed = feed,
      key = companyIdS,
      load = companyId =>
        Api.dashboards
          .getDirectoryDashboard(CompanyId(companyId))
          .map(response => (response, response.globalPosition)),
      triggerRefresh = message =>
        DirectoryDashboardEvents.types.contains(message.eventType)
    )
end LiveDirectoryDashboard
