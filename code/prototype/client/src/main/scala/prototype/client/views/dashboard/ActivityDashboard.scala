package prototype.client.views.dashboard

import java.util.UUID

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import prototype.api.*
import prototype.api.PeriodUnit.*
import prototype.client.ErrorMessages
import prototype.client.dashboards.{
  ComputationLevel,
  DashboardSources,
  LiveActivityDashboard
}
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.views.components.*
import prototype.client.views.components.chartJS.*

import com.raquo.laminar.api.L.*
import smithy4s.time.LocalDate

object ActivityDashboard:

  def apply(
      sources: DashboardSources,
      feed: LiveFeed,
      companyIdS: Signal[UUID],
      computationLevelS: Signal[ComputationLevel]
  ): HtmlElement =
    val selectedPeriodUnitVar: Var[PeriodUnit] = Var(PeriodUnit.WEEK)
    val selectedPeriodDayVar: Var[LocalDate] = Var(LocalDate.now())

    val dashboard: LiveData[(PeriodUnit, ActivityFigures)] =
      LiveData.switching(computationLevelS) {
        case ComputationLevel.Browser =>
          LiveActivityDashboard.replayed(
            sources,
            selectedPeriodUnitVar.signal,
            selectedPeriodDayVar.signal
          )
        case ComputationLevel.Server =>
          LiveActivityDashboard(
            feed,
            companyIdS,
            selectedPeriodUnitVar.signal,
            selectedPeriodDayVar.signal
          )
      }

    // The period shown is the one the figures were computed for
    val periodS: Signal[Option[(LocalDate, LocalDate)]] =
      dashboard.valueS.map(_.map((_, out) => (out.periodStart, out.periodEnd)))

    div(
      cls := "dash",
      renderPeriodSelector(
        selectedPeriodUnitVar,
        selectedPeriodDayVar,
        periodS
      ),
      child.maybe <-- dashboard.errorS.map(
        _.map(error =>
          div(cls := "dash__error", ErrorMessages.errorToMessage(error))
        )
      ),
      child <-- dashboard.valueS.splitOption(
        (_, answerS) => renderAnswer(answerS),
        ifEmpty = p(cls := "dash__empty", "Loading activity dashboard...")
      )
    )
  end apply

  // Return the period picker (the period unit picker and the range picker)
  private def renderPeriodSelector(
      selectedPeriodUnitVar: Var[PeriodUnit],
      selectedPeriodDayVar: Var[LocalDate],
      periodS: Signal[Option[(LocalDate, LocalDate)]]
  ): HtmlElement =
    def stepBy(periods: Int): Mod[Button] =
      onClick.compose(_.sample(selectedPeriodUnitVar.signal)) --> { unit =>
        selectedPeriodDayVar.update(day => stepDay(day, unit, periods))
      }
    div(
      cls := "dash__toolbar",
      PopoverPicker(
        id = "period-unit-picker",
        label = "Period Unit",
        items = Val(
          List(
            PopoverPicker.Item(WEEK, "Week"),
            PopoverPicker.Item(MONTH, "Month")
          )
        ),
        selectedVar = selectedPeriodUnitVar
      ),
      button(
        typ := "button",
        cls := "btn",
        aria.label := "Previous period",
        "‹",
        stepBy(-1)
      ),
      span(
        cls := "dash__period",
        child.text <-- periodS.map {
          case Some((start, end)) => s"$start to $end"
          case None               => "..."
        }
      ),
      button(
        typ := "button",
        cls := "btn",
        aria.label := "Next period",
        "›",
        stepBy(1)
      )
    )
  end renderPeriodSelector

  // The same day, some `PeriodUnit` later
  private def stepDay(day: LocalDate, unit: PeriodUnit, by: Int): LocalDate =
    val javaDay = java.time.LocalDate.ofEpochDay(day.epochDay)
    val stepped = unit match
      case WEEK  => javaDay.plusWeeks(by)
      case MONTH => javaDay.plusMonths(by)
    LocalDate(stepped.toEpochDay)

  // The charts get a signal of their config, not a fixed one: they stay on
  // the page and update() when the period changes
  private def renderAnswer(
      dashboardS: Signal[(PeriodUnit, ActivityFigures)]
  ): HtmlElement =
    val outS = dashboardS.map((_, out) => out)
    div(
      cls := "dash",
      div(
        cls := "dash__stats",
        StatBox("Appointments", outS.map(_.appointments.toString)),
        StatBox("Patients seen", outS.map(_.patientsSeen.toString)),
        StatBox("Busiest therapist", outS.map(busiestTherapistText))
      ),
      div(
        cls := "dash__grid",
        div(
          cls := "dash__card panel",
          h3(cls := "dash__card-title", "Appointments per Therapist"),
          chartCanvas(outS.map(perTherapistChartConfig))
        ),
        div(
          cls := "dash__card panel",
          h3(cls := "dash__card-title", "Evolution"),
          chartCanvas(
            dashboardS.map((unit, out) => evolutionChartConfig(unit, out))
          )
        )
      )
    )
  end renderAnswer

  private def busiestTherapistText(out: ActivityFigures): String =
    out.busiestTherapist.fold("None")(t =>
      s"${t.therapistId.value} (${t.count} appointment${
          if t.count == 1 then "" else "s"
        })"
    )

  private def perTherapistChartConfig(
      out: ActivityFigures
  ): ChartConfig =
    val therapistCounts = out.perTherapist.sortBy(-_.count)
    new ChartConfig:
      val `type` = "bar"
      override val options = integerAxisOptions()
      val data = new ChartData:
        val labels = therapistCounts.map(_.therapistId.value).toJSArray
        val datasets = js.Array(new ChartDataset:
          val label = "Appointments per Therapist"
          val data = therapistCounts.map(_.count.toDouble).toJSArray)
  end perTherapistChartConfig

  // The chosen period and the 5 before it, oldest first
  private def evolutionChartConfig(
      unit: PeriodUnit,
      out: ActivityFigures
  ): ChartConfig =
    new ChartConfig:
      val `type` = "line"
      override val options = integerAxisOptions()
      val data = new ChartData:
        val labels =
          out.evolution.map(p => periodLabel(unit, p.start)).toJSArray
        val datasets = js.Array(new ChartDataset:
          val label = "Appointments"
          val data = out.evolution.map(_.appointments.toDouble).toJSArray)
  end evolutionChartConfig

  // Week: "12.10" (day.month of its Monday); month: "10.2026"
  private def periodLabel(unit: PeriodUnit, start: LocalDate): String =
    val iso = start.toString // yyyy-MM-dd
    unit match
      case WEEK  => s"${iso.substring(8, 10)}.${iso.substring(5, 7)}"
      case MONTH => s"${iso.substring(5, 7)}.${iso.substring(0, 4)}"
end ActivityDashboard
