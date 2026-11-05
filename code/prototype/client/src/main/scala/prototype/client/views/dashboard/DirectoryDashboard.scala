package prototype.client.views.dashboard

import java.util.UUID

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import prototype.api.*
import prototype.client.ErrorMessages
import prototype.client.dashboards.LiveDirectoryDashboard
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.views.components.StatBox
import prototype.client.views.components.chartJS.*

import com.raquo.laminar.api.L.*

object DirectoryDashboard:
  def apply(selectedCompanyS: Signal[Option[UUID]]): HtmlElement =
    div(
      cls := "dash",
      child <-- selectedCompanyS.splitOption(
        (_, companyIdS) => renderCompanyDashboard(companyIdS),
        div(
          cls := "dash__empty",
          "Select a company to see the directory dashboard."
        )
      )
    )
  end apply

  // Built once a company is selected, and kept while it changes. The feed is
  // opened here, not above: a live value first read after the stream opened
  // would never load
  private def renderCompanyDashboard(companyIdS: Signal[UUID]): HtmlElement =
    val feed: EventBus[FeedMessage] = new EventBus[FeedMessage]

    // A new company drops the figures shown until the new ones arrive
    val dashboard: LiveData[GetDirectoryDashboardOutput] =
      LiveDirectoryDashboard(feed, companyIdS)

    div(
      cls := "dash",
      LiveFeed.bind(feed),
      child.maybe <-- dashboard.errorS.map(
        _.map(error =>
          div(cls := "dash__error", ErrorMessages.errorToMessage(error))
        )
      ),
      child <-- dashboard.valueS.splitOption(
        (_, dataS) => renderDirectoryDashboard(dataS),
        div(cls := "dash__empty", "Loading directory dashboard...")
      )
    )
  end renderCompanyDashboard

  // Takes a signal, not a fixed value: on a live update the figures and the
  // charts change in place instead of being built again
  private def renderDirectoryDashboard(
      dataS: Signal[GetDirectoryDashboardOutput]
  ): HtmlElement =
    div(
      cls := "dash",
      div(
        cls := "dash__stats",
        StatBox(
          statLabel = "Total Therapists",
          valueS = dataS.map(_.therapists.toString)
        ),
        StatBox(
          statLabel = "Patients Seen",
          valueS = dataS.map(_.patientsSeen.toString)
        ),
        StatBox(
          statLabel = "Active Patients",
          valueS = dataS.map(_.activePatients.toString)
        ),
        StatBox(
          statLabel = "Total Appointments",
          valueS = dataS.map(_.appointments.toString)
        )
      ),
      div(
        cls := "dash__grid",
        div(
          cls := "dash__card panel",
          h3(cls := "dash__card-title", "Therapists per Role"),
          chartCanvas(
            dataS.map(d => therapistsPerRoleChartConfig(d.therapistsPerRole))
          )
        ),
        div(
          cls := "dash__card panel",
          h3(cls := "dash__card-title", "Patients per Therapist"),
          chartCanvas(
            dataS.map(d =>
              patientsPerTherapistChartConfig(d.patientsPerTherapist)
            )
          )
        )
      )
    )
  end renderDirectoryDashboard

  private def therapistsPerRoleChartConfig(
      roleCounts: List[RoleCount]
  ): ChartConfig =
    new ChartConfig:
      val `type` = "bar"
      override val options = integerAxisOptions()
      val data = new ChartData:
        val labels = roleCounts
          .map(
            _.role.toString.replace("_", " ").toLowerCase.capitalize
          )
          .toJSArray
        val datasets = js.Array(new ChartDataset:
          val label = "Therapists per Role"
          val data = roleCounts.map(_.count.toDouble).toJSArray)
  end therapistsPerRoleChartConfig

  private def patientsPerTherapistChartConfig(
      therapistCounts: List[TherapistCount]
  ): ChartConfig =

    val top5Therapists = therapistCounts
      .sortBy(-_.count)
      .take(5)

    new ChartConfig:
      val `type` = "bar"
      override val options = integerAxisOptions()
      val data = new ChartData:
        val labels = top5Therapists.map(_.therapistId.value).toJSArray
        val datasets = js.Array(new ChartDataset:
          val label = "Patients per Therapist"
          val data = top5Therapists.map(_.count.toDouble).toJSArray)
  end patientsPerTherapistChartConfig
