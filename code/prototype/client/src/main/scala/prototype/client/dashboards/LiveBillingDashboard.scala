package prototype.client.dashboards

import java.time.Instant
import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, RefreshedLiveData, TraceStep}
import prototype.shared.dashboards.{BillingDashboard, BillingDashboardEvents}

import com.raquo.laminar.api.L.*

/** The billing dashboard of one company, either reloaded from the server or
  * computed in the browser.
  */
object LiveBillingDashboard:

  /** Its name in the trace, read by the step-by-step mode too. */
  val name: String = "billing dashboard"

  // The name of the browser's figures compared with the server's
  private val CheckName: String = s"$name, browser vs server"

  def apply(
      feed: LiveFeed,
      companyIdS: Signal[UUID]
  ): LiveData[BillingFigures] =
    new RefreshedLiveData[UUID, BillingFigures](
      feed = feed,
      key = companyIdS,
      load = companyId =>
        Api.dashboards
          .getBillingDashboard(CompanyId(companyId))
          .map(response => (response.dashboard, response.globalPosition)),
      triggerRefresh = message =>
        BillingDashboardEvents.types.contains(message.eventType),
      name = name
    )

  /** Computed in the browser from the company's live appointments, with the
    * shared `BillingDashboard`: no request when an event arrives. Its status
    * also compares it with the server's figures, once things settle.
    */
  def replayed(
      feed: LiveFeed,
      companyIdS: Signal[UUID],
      companyAppointments: LiveData[List[Appointment]]
  ): LiveData[BillingFigures] =
    companyAppointments
      .withCurrentValueOf(feed.lastPositionS)
      .map((appointments, position) =>
        feed.tracer.timed[BillingFigures](
          figures => TraceStep.Kind.FiguresComputed(name, figures),
          Option.when(position >= 0)(position)
        )(
          BillingDashboard(appointments, Instant.now())
        )
      )
      .checkedAgainst(
        feed,
        CheckName,
        companyIdS,
        companyId =>
          Api.dashboards
            .getBillingDashboard(CompanyId(companyId))
            .map(_.dashboard)
      )
end LiveBillingDashboard
