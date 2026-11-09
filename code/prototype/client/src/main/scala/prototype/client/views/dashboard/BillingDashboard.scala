package prototype.client.views.dashboard

import java.util.UUID

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import prototype.api.*
import prototype.client.ErrorMessages
import prototype.client.dashboards.LiveBillingDashboard
import prototype.client.live.{LiveData, LiveFeed}
import prototype.client.views.components.*
import prototype.client.views.components.chartJS.*

import com.raquo.laminar.api.L.*
import smithy4s.time.LocalDate

object BillingDashboard:

  /** Return the billing dashboard component
    *
    * @param selectedCompanyS
    *   the signal of the selected company, None if no company is selected
    * @return
    *   the real time billing dashboard
    */
  def apply(
      feed: LiveFeed,
      selectedCompanyS: Signal[Option[UUID]]
  ): HtmlElement =
    div(
      cls := "dash",
      child <-- selectedCompanyS.splitOption(
        (_, companyIdS) => renderCompanyDashboard(feed, companyIdS),
        ifEmpty = p(
          cls := "dash__empty",
          "Select a company to see the billing dashboard."
        )
      )
    )
  end apply

  // Built once a company is selected, and kept while it changes.
  private def renderCompanyDashboard(
      feed: LiveFeed,
      companyIdS: Signal[UUID]
  ): HtmlElement =
    // The billing dashboard
    val dashboard: LiveData[GetBillingDashboardOutput] =
      LiveBillingDashboard(feed, companyIdS)

    div(
      cls := "dash",
      child.maybe <-- dashboard.errorS.map(
        _.map(error =>
          div(cls := "dash__error", ErrorMessages.errorToMessage(error))
        )
      ),
      child <-- dashboard.valueS.splitOption(
        (_, dataS) => renderBillingDashboard(dataS),
        ifEmpty = p(cls := "dash__empty", "Loading billing dashboard...")
      )
    )
  end renderCompanyDashboard

  // Takes a signal, so that on a live update the figures and the
  // chart change in place instead of being built again
  private def renderBillingDashboard(
      dataS: Signal[GetBillingDashboardOutput]
  ): HtmlElement =
    div(
      cls := "dash",
      div(
        cls := "dash__stats",
        StatBox("To bill", dataS.map(d => chf(d.amountPerStatus.toBill))),
        StatBox("Billed", dataS.map(d => chf(d.amountPerStatus.billed))),
        StatBox("Paid", dataS.map(d => chf(d.amountPerStatus.paid))),
        StatBox("Cancelled", dataS.map(d => chf(d.amountPerStatus.cancelled)))
      ),
      div(
        cls := "dash__card panel",
        h3(cls := "dash__card-title", "Oldest Appointments to Bill"),
        child <-- dataS.map(d => renderOldestToBill(d.oldestToBill))
      ),
      div(
        cls := "dash__card panel",
        h3(cls := "dash__card-title", "Cumulative Amounts, Last 52 Weeks"),
        chartCanvas(dataS.map(d => weeklyChartConfig(d.weekly)))
      )
    )
  end renderBillingDashboard

  // A table showing the oldest prestations to bill
  private def renderOldestToBill(items: List[ToBillItem]): HtmlElement =
    if items.isEmpty then p(cls := "dash__empty", "Nothing left to bill.")
    else
      div(
        cls := "dash__scroll",
        table(
          cls := "dash__table",
          thead(
            tr(
              th("Date"),
              th("Type"),
              th("Therapists"),
              th(cls := "dash__num", "To bill")
            )
          ),
          tbody(
            items.map(item =>
              tr(
                td(item.date.toString),
                td(readable(item.appointmentType.toString)),
                td(item.therapistIds.map(_.value).mkString(", ")),
                td(cls := "dash__num", chf(item.amount))
              )
            )
          )
        )
      )

  // Amounts are written the Swiss way: CHF 1'250.50
  private def chf(amount: Float): String =
    val formatted = amount.toDouble
      .asInstanceOf[js.Dynamic]
      .toLocaleString(
        "de-CH",
        js.Dynamic.literal(minimumFractionDigits = 2, maximumFractionDigits = 2)
      )
      .asInstanceOf[String]
    s"CHF $formatted"

  private def readable(name: String): String =
    name.replace("_", " ").toLowerCase.capitalize

  // Running totals from the first of the 52 weeks, so every line only goes up.
  private def weeklyChartConfig(weekly: List[WeeklyAmount]): ChartConfig =
    val paid = runningTotal(weekly.map(_.paid.toDouble))
    val paidOrBilled =
      runningTotal(weekly.map(w => (w.paid + w.billed).toDouble))
    val everything =
      runningTotal(weekly.map(w => (w.paid + w.billed + w.toBill).toDouble))
    new ChartConfig:
      val `type` = "line"
      override val options = weeklyChartOptions()
      val data = new ChartData:
        val labels = weekly.map(w => weekLabel(w.weekStart)).toJSArray
        val datasets = js.Array(
          lineDataset("Paid", paid, "#0f766e"),
          lineDataset("Paid + billed", paidOrBilled, "#2bb3a3"),
          lineDataset("Paid + billed + to bill", everything, "#d9a21b")
        )
  end weeklyChartConfig

  private def runningTotal(values: List[Double]): List[Double] =
    values.scanLeft(0.0)(_ + _).tail

  private def lineDataset(
      name: String,
      values: List[Double],
      color: String
  ): ChartDataset =
    new ChartDataset:
      val label = name
      val data = values.toJSArray
      override val borderColor = color
      override val backgroundColor = color

  // 52 points: no dot on each one, and a tooltip with the three values of a week
  private def weeklyChartOptions(): js.Object =
    js.Dynamic.literal(
      scales = js.Dynamic.literal(
        y = js.Dynamic.literal(beginAtZero = true)
      ),
      elements = js.Dynamic.literal(
        point = js.Dynamic.literal(radius = 0, hitRadius = 8)
      ),
      interaction = js.Dynamic.literal(mode = "index", intersect = false)
    )

  // The Monday of the week, as "12.10"
  private def weekLabel(weekStart: LocalDate): String =
    val iso = weekStart.toString // yyyy-MM-dd
    s"${iso.substring(8, 10)}.${iso.substring(5, 7)}"
end BillingDashboard
