package prototype.client.views.dashboard

import java.util.UUID

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import prototype.api.*
import prototype.client.{
  Api,
  ErrorMessages
}
import prototype.client.Api.ApiCall
import prototype.client.views.components.StatBox
import prototype.client.views.components.chartJS.*

import com.raquo.laminar.api.L.*

object DirectoryDashboard:
  def apply(selectedCompanyS: Signal[Option[UUID]]): HtmlElement =
    val dataS: ApiCall[GetDirectoryDashboardOutput] =
      ApiCall(
        selectedCompanyS.flatMapSwitch {
          case Some(companyId) =>
            ApiCall
              .fromIOCallSimple(
                Api.dashboards.getDirectoryDashboard(CompanyId(companyId))
              )
              .stateS
          case None => Val(Api.Loading)
        }
      )
    div(
      child <-- dataS.stateS.combineWith(selectedCompanyS).map {
        case (Api.Loading, None) =>
          div(
            cls := "dash__empty",
            "Select a company to see the directory dashboard."
          )
        case (Api.Loading, Some(_)) =>
          div(
            cls := "dash__empty",
            "Loading directory dashboard..."
          )
        case (Api.Error(error), _) =>
          div(cls := "dash__error", ErrorMessages.errorToMessage(error))
        case (Api.Success(data), _) =>
          renderDirectoryDashboard(data)
      }
    )
  end apply

  private def renderDirectoryDashboard(
      data: GetDirectoryDashboardOutput
  ): HtmlElement =
    div(
      cls := "dash",
      div(
        cls := "dash__stats",
        StatBox(
          statLabel = "Total Therapists",
          valueS = Val(data.therapists.toString)
        ),
        StatBox(
          statLabel = "Patients Seen",
          valueS = Val(data.patientsSeen.toString)
        ),
        StatBox(
          statLabel = "Active Patients",
          valueS = Val(data.activePatients.toString)
        ),
        StatBox(
          statLabel = "Total Appointments",
          valueS = Val(data.appointments.toString)
        )
      ),
      div(
        cls := "dash__grid",
        div(
          cls := "dash__card panel",
          h3(cls := "dash__card-title", "Therapists per Role"),
          chartCanvas(
            Val(therapistsPerRoleChartConfig(data.therapistsPerRole))
          )
        ),
        div(
          cls := "dash__card panel",
          h3(cls := "dash__card-title", "Patients per Therapist"),
          chartCanvas(
            Val(patientsPerTherapistChartConfig(data.patientsPerTherapist))
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
