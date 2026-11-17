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

  /** Its name in the trace, read by the step-by-step mode too. */
  val name: String = "billing dashboard"

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
    * shared `BillingDashboard`: no request when an event arrives.
    */
  def replayed(
      tracer: Tracer,
      processedEventS: Signal[Long],
      companyAppointments: LiveData[List[Appointment]]
  ): LiveData[BillingFigures] =
    companyAppointments
      .withCurrentValueOf(processedEventS)
      .map((appointments, position) =>
        tracer.timed[BillingFigures](
          figures => TraceStep.Kind.FiguresComputed(name, figures),
          Option.when(position >= 0)(position)
        )(
          BillingDashboard(appointments, Instant.now())
        )
      )
end LiveBillingDashboard
