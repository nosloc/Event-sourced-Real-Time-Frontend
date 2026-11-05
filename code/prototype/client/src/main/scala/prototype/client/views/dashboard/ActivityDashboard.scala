package prototype.client.views.dashboard

import java.util.UUID

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import prototype.api.*
import prototype.api.PeriodUnit.*
import prototype.client.ErrorMessages
import prototype.client.dashboards.LiveActivityDashboard
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.live.LiveFeed.FeedMessage
import prototype.client.views.components.*
import prototype.client.views.components.chartJS.*

import com.raquo.laminar.api.L.*
import smithy4s.time.LocalDate

object ActivityDashboard:

  // What the server answered, with the unit it was asked for: the evolution
  // chart labels its points by week or by month
  private type Answer = (PeriodUnit, GetActivityDashboardOutput)

  def apply(selectedCompanySignal: Signal[Option[UUID]]): HtmlElement =
    val selectedPeriodUnitVar: Var[PeriodUnit] = Var(WEEK)

    // Any day inside the period shown: the server works out the week or month
    val selectedPeriodDayVar: Var[LocalDate] = Var(LocalDate.now())

    div(
      cls := "dash",
      child <-- selectedCompanySignal.splitOption(
        (_, companyIdS) =>
          renderCompanyDashboard(
            companyIdS,
            selectedPeriodUnitVar,
            selectedPeriodDayVar
          ),
        p(
          cls := "dash__empty",
          "Select a company to see the activity dashboard."
        )
      )
    )
  end apply

  // Built once a company is selected, and kept while it changes. The feed is
  // opened here, not above: a live value first read after the stream opened
  // would never load
  private def renderCompanyDashboard(
      companyIdS: Signal[UUID],
      selectedPeriodUnitVar: Var[PeriodUnit],
      selectedPeriodDayVar: Var[LocalDate]
  ): HtmlElement =
    val feed: EventBus[FeedMessage] = new EventBus[FeedMessage]

    // A new company or period drops the figures shown until the new ones arrive
    val dashboard: LiveData[Answer] = LiveActivityDashboard(
      feed,
      companyIdS,
      selectedPeriodUnitVar.signal,
      selectedPeriodDayVar.signal
    )

    // The period shown is the one the server answered with: the arrows step
    // from its first and last day, so months of different lengths need no maths
    val periodS: Signal[Option[(LocalDate, LocalDate)]] =
      dashboard.valueS.map(_.map((_, out) => (out.periodStart, out.periodEnd)))

    div(
      cls := "dash",
      LiveFeed.bind(feed),
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
        p(cls := "dash__empty", "Loading activity dashboard...")
      )
    )
  end renderCompanyDashboard

  private def renderPeriodSelector(
      selectedPeriodUnitVar: Var[PeriodUnit],
      selectedPeriodDayVar: Var[LocalDate],
      periodS: Signal[Option[(LocalDate, LocalDate)]]
  ): HtmlElement =
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
        disabled <-- periodS.map(_.isEmpty),
        onClick.compose(_.sample(periodS)) --> {
          // The day before the period is in the previous one
          case Some((start, _)) =>
            selectedPeriodDayVar.set(LocalDate(start.epochDay - 1))
          case None => ()
        }
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
        disabled <-- periodS.map(_.isEmpty),
        onClick.compose(_.sample(periodS)) --> {
          // The day after the period is in the next one
          case Some((_, end)) =>
            selectedPeriodDayVar.set(LocalDate(end.epochDay + 1))
          case None => ()
        }
      )
    )
  end renderPeriodSelector

  // The charts get a signal of their config, not a fixed one: they stay on
  // the page and update() when the period changes
  private def renderAnswer(answerS: Signal[Answer]): HtmlElement =
    val outS = answerS.map((_, out) => out)
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
            answerS.map((unit, out) => evolutionChartConfig(unit, out))
          )
        )
      )
    )
  end renderAnswer

  private def busiestTherapistText(out: GetActivityDashboardOutput): String =
    out.busiestTherapist.fold("None")(t =>
      s"${t.therapistId.value} (${t.count} appointment${
          if t.count == 1 then "" else "s"
        })"
    )

  private def perTherapistChartConfig(
      out: GetActivityDashboardOutput
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
      out: GetActivityDashboardOutput
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
