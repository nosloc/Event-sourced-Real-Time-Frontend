package prototype.client.dashboards

import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData, TraceStep}
import prototype.shared.dashboards.{ActivityDashboard, ActivityDashboardEvents}

import com.raquo.laminar.api.L.*
import smithy4s.time.LocalDate

/** The activity dashboard of one company, either reloaded from the server or
  * computed in the browser. The figures come with the unit they are for: the
  * evolution chart labels its points by week or by month.
  */
object LiveActivityDashboard:

  /** Its name in the trace, read by the step-by-step mode too. */
  val name: String = "activity dashboard"

  // The name of the browser's figures compared with the server's
  private val CheckName: String = s"$name, browser vs server"

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
        oldKey.companyId != newKey.companyId,
      name = name
    )

  /** Computed in the browser from the company's live therapists and
    * appointments and the chosen period, with the shared `ActivityDashboard`:
    * no request when an event arrives or the period changes. Its status also
    * compares it with the server's figures, once things settle.
    */
  def replayed(
      feed: LiveFeed,
      companyIdS: Signal[UUID],
      sources: DashboardSources,
      unitS: Signal[PeriodUnit],
      dateS: Signal[LocalDate]
  ): LiveData[(PeriodUnit, ActivityFigures)] =
    sources
      .combineWith(unitS.combineWith(dateS))
      .withCurrentValueOf(feed.lastPositionS)
      .map { case (((therapists, appointments), (unit, date)), position) =>
        feed.tracer.timed[(PeriodUnit, ActivityFigures)](
          all => TraceStep.Kind.FiguresComputed(name, all),
          Option.when(position >= 0)(position)
        )(
          (unit, ActivityDashboard(therapists, appointments, unit, date))
        )
      }
      .checkedAgainst(
        feed,
        CheckName,
        companyIdS.combineWith(unitS, dateS).map(Key.apply),
        key =>
          Api.dashboards
            .getActivityDashboard(
              companyId = CompanyId(key.companyId),
              unit = key.periodUnit,
              date = key.periodDay
            )
            .map(response => (key.periodUnit, response.dashboard))
      )
end LiveActivityDashboard
