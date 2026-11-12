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
import prototype.shared.dashboards.{
  DirectoryDashboard,
  DirectoryDashboardEvents
}

import com.raquo.laminar.api.L.*

/** The directory dashboard of one company, either reloaded from the server or
  * computed in the browser.
  */
object LiveDirectoryDashboard:

  /** Its name in the trace, read by the step-by-step mode too. */
  val name: String = "directory dashboard"

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
        DirectoryDashboardEvents.types.contains(message.eventType),
      name = name
    )

  /** Computed in the browser from the company's live therapists and
    * appointments, with the shared `DirectoryDashboard`: no request when an
    * event arrives.
    */
  def replayed(
      tracer: Tracer,
      processedEvent: Signal[Long],
      sources: DashboardSources
  ): LiveData[DirectoryFigures] =
    sources
      .withCurrentValueOf(processedEvent)
      .map { case ((therapists, appointments), position) =>
        tracer.timed(
          TraceStep.Kind.FiguresComputed("directory"),
          Option.when(position >= 0)(position)
        )(
          DirectoryDashboard(
            therapists,
            appointments,
            Instant.now()
          )
        )
      }
end LiveDirectoryDashboard
