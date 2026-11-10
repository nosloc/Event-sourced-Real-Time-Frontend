package prototype.client.dashboards

import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData}
import prototype.shared.dashboards.{ActivityDashboard, ActivityDashboardEvents}

import com.raquo.laminar.api.L.*
import smithy4s.time.LocalDate

/** The activity dashboard of one company, either reloaded from the server or
  * computed in the browser. The figures come with the unit they are for: the
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
  ): LiveData[(PeriodUnit, ActivityFigures)] =
    val keyS: Signal[Key] =
      companyIdS.combineWith(periodUnitS, periodDayS).map(Key.apply)
    new RefreshedLiveData[
      Key,
      (PeriodUnit, ActivityFigures)
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
            ((key.periodUnit, response.dashboard), response.globalPosition)
          ),
      triggerRefresh = message =>
        ActivityDashboardEvents.types.contains(message.eventType),
      invalidatePrevious = (oldKey, newKey) =>
        oldKey.companyId != newKey.companyId
    )

  /** Computed in the browser from the company's live therapists and
    * appointments and the chosen period, with the shared `ActivityDashboard`:
    * no request when an event arrives or the period changes.
    */
  def replayed(
      sources: DashboardSources,
      unitS: Signal[PeriodUnit],
      dateS: Signal[LocalDate]
  ): LiveData[(PeriodUnit, ActivityFigures)] =
    sources
      .combineWithFn(unitS.combineWith(dateS)) {
        case ((therapists, appointments), (unit, date)) =>
          (unit, ActivityDashboard(therapists, appointments, unit, date))
      }
end LiveActivityDashboard
