package prototype.client.dashboards

import java.time.Instant
import java.util.UUID

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{
  LiveData,
  LiveFeed,
  RefreshedLiveData,
  TraceStep,
  Tracer
}
import prototype.shared.dashboards.{BillingDashboard, BillingDashboardEvents}

import com.raquo.laminar.api.L.*

/** The billing dashboard of one company, either reloaded from the server or
  * computed in the browser.
  */
object LiveBillingDashboard:

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
      name = "billing dashboard"
    )

  /** Computed in the browser from the company's live appointments, with the
    * shared `BillingDashboard`: no request when an event arrives.
    */
  def replayed(
      tracer: Tracer,
      processedEvent: Signal[Long],
      companyAppointments: LiveData[List[Appointment]]
  ): LiveData[BillingFigures] =
    companyAppointments
      .withCurrentValueOf(processedEvent)
      .map((appointments, position) =>
        tracer.timed(
          TraceStep.Kind.FiguresComputed("billing"),
          Option.when(position >= 0)(position)
        )(
          BillingDashboard(appointments, Instant.now())
        )
      )
end LiveBillingDashboard
