package prototype.client.dashboards

import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, RefreshedLiveData}
import prototype.client.live.LiveFeed.FeedMessage
import prototype.shared.events.DomainEvent

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
      feed: EventBus[FeedMessage],
      companyIdS: Signal[UUID],
      periodUnitS: Signal[PeriodUnit],
      periodDayS: Signal[LocalDate]
  ): LiveData[(PeriodUnit, GetActivityDashboardOutput)] =
    // TODO: Only the id should invalidate previous data, not the period or the day
    val keyS: Signal[Key] =
      companyIdS.combineWith(periodUnitS, periodDayS).map(Key.apply)

    new RefreshedLiveData[
      Key,
      (PeriodUnit, GetActivityDashboardOutput),
      DomainEvent
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
          // TODO: global position is not returned by the API yet, so we just use 0L for now
          .map(response => ((key.periodUnit, response), 0L)),
      triggerRefresh = _ => true // TODO: for now every event triggers a refresh
    )
end LiveActivityDashboard
