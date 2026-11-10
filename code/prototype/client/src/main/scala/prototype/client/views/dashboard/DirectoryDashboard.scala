package prototype.client.views.dashboard

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import prototype.api.*
import prototype.client.ErrorMessages
import prototype.client.dashboards.{DashboardSources, LiveDirectoryDashboard}
import prototype.client.live.LiveData
import prototype.client.views.components.StatBox
import prototype.client.views.components.chartJS.*

import com.raquo.laminar.api.L.*

object DirectoryDashboard:
  def apply(
      sources: DashboardSources
  ): HtmlElement =
    // A new company drops the figures shown until its appointments are loaded
    // (its therapists are filtered at once)
    val dashboard: LiveData[DirectoryFigures] =
      LiveDirectoryDashboard.replayed(sources)

    div(
      cls := "dash",
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
  end apply

  // Takes a signal, so that on a live update the figures and the
  // charts change in place instead of being built again
  private def renderDirectoryDashboard(
      dataS: Signal[DirectoryFigures]
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
