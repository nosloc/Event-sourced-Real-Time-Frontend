package prototype.client.dashboards

import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData}
import prototype.shared.dashboards.ActivityDashboardEvents

import com.raquo.laminar.api.L.*
import smithy4s.time.LocalDate

/** The activity dashboard of one company, reloaded from the server when an
  * event arrives. The answer comes with the unit it was asked for: the
  * evolution chart labels its points by week or by month.
  */
object LiveActivityDashboard:

  private case class Key(
      companyId: UUID,
      periodUnit: PeriodUnit,
      periodDay: LocalDate
  )

  def apply(
      feed: LiveFeed,
      companyIdS: Signal[UUID],
      periodUnitS: Signal[PeriodUnit],
      periodDayS: Signal[LocalDate]
  ): LiveData[(PeriodUnit, GetActivityDashboardOutput)] =
    val keyS: Signal[Key] =
      companyIdS.combineWith(periodUnitS, periodDayS).map(Key.apply)
    new RefreshedLiveData[
      Key,
      (PeriodUnit, GetActivityDashboardOutput)
    ](
      feed = feed,
      key = keyS,
      load = key =>
        Api.dashboards
          .getActivityDashboard(
            companyId = CompanyId(key.companyId),
            unit = key.periodUnit,
            date = key.periodDay
          )
          .map(response =>
            ((key.periodUnit, response), response.globalPosition)
          ),
      triggerRefresh = message =>
        ActivityDashboardEvents.types.contains(message.eventType),
      invalidatePrevious = (oldKey, newKey) =>
        oldKey.companyId != newKey.companyId
    )
end LiveActivityDashboard
