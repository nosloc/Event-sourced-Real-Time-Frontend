package prototype.client.dashboards

import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData}
import prototype.shared.dashboards.BillingDashboardEvents

import com.raquo.laminar.api.L.*

/** The billing dashboard of one company, reloaded from the server when one of
  * its events arrives.
  */
object LiveBillingDashboard:

  def apply(
      feed: LiveFeed,
      companyIdS: Signal[UUID]
  ): LiveData[GetBillingDashboardOutput] =
    new RefreshedLiveData[UUID, GetBillingDashboardOutput](
      feed = feed,
      key = companyIdS,
      load = companyId =>
        Api.dashboards
          .getBillingDashboard(CompanyId(companyId))
          .map(response => (response, response.globalPosition)),
      triggerRefresh = message =>
        BillingDashboardEvents.types.contains(message.eventType)
    )
end LiveBillingDashboard
