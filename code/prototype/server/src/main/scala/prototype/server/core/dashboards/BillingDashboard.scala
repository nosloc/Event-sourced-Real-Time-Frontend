package prototype.server.core.dashboards

import java.time.{DayOfWeek, Instant, LocalDate as JavaLocalDate, ZoneId}
import java.time.temporal.TemporalAdjusters

import prototype.api.*

import smithy4s.time.LocalDate

object BillingDashboard:
  // Weeks and days are cut in one fixed zone, whatever the machine's zone is
  private val zone = ZoneId.of("Europe/Zurich")
  private val weekCount = 52
  private val oldestCount = 5

  def apply(
      companyAppointments: List[Appointment],
      now: Instant
  ): GetBillingDashboardOutput =
    GetBillingDashboardOutput(
      amountPerStatus = getAmountPerStatus(companyAppointments),
      oldestToBill = getOldestToBill(companyAppointments),
      weekly = getWeeklyAmounts(companyAppointments, now)
    )
  end apply

  private def amount(prestation: Prestation): Double =
    prestation.quantity.toDouble * prestation.unitPrice

  private def amountOf(
      prestations: List[Prestation],
      status: PrestationStatus
  ): Float =
    prestations.filter(_.status == status).map(amount).sum.toFloat

  private def getAmountPerStatus(
      companyAppointments: List[Appointment]
  ): AmountPerStatus =
    val prestations = companyAppointments.flatMap(_.prestations)
    AmountPerStatus(
      toBill = amountOf(prestations, PrestationStatus.TO_BILL),
      billed = amountOf(prestations, PrestationStatus.BILLED),
      paid = amountOf(prestations, PrestationStatus.PAID),
      cancelled = amountOf(prestations, PrestationStatus.CANCELLED)
    )
  end getAmountPerStatus

  // The oldest appointments that still have something to bill
  private def getOldestToBill(
      companyAppointments: List[Appointment]
  ): List[ToBillItem] =
    companyAppointments
      .filter(_.prestations.exists(_.status == PrestationStatus.TO_BILL))
      .sortBy(_.timeRange.start.epochSecond)
      .take(oldestCount)
      .map(appt =>
        ToBillItem(
          appointmentId = appt.appointmentId,
          date = toSmithyDate(dayOf(appt)),
          appointmentType = appt.appointmentType,
          therapistIds = appt.therapistIds,
          amount = amountOf(appt.prestations, PrestationStatus.TO_BILL)
        )
      )
  end getOldestToBill

  // One entry per week for the last weeks ending with the current one, oldest
  // first. A prestation counts on the day of its appointment, whatever its
  // status; cancelled ones are left out.
  private def getWeeklyAmounts(
      companyAppointments: List[Appointment],
      now: Instant
  ): List[WeeklyAmount] =
    val currentWeek = mondayOf(now.atZone(zone).toLocalDate)
    val byWeek = companyAppointments.groupBy(appt => mondayOf(dayOf(appt)))
    (weekCount - 1 to 0 by -1).toList.map { weeksAgo =>
      val week = currentWeek.minusWeeks(weeksAgo.toLong)
      val prestations = byWeek.getOrElse(week, Nil).flatMap(_.prestations)
      WeeklyAmount(
        weekStart = toSmithyDate(week),
        toBill = amountOf(prestations, PrestationStatus.TO_BILL),
        billed = amountOf(prestations, PrestationStatus.BILLED),
        paid = amountOf(prestations, PrestationStatus.PAID)
      )
    }
  end getWeeklyAmounts

  private def dayOf(appointment: Appointment): JavaLocalDate =
    Instant
      .ofEpochSecond(appointment.timeRange.start.epochSecond)
      .atZone(zone)
      .toLocalDate

  private def mondayOf(day: JavaLocalDate): JavaLocalDate =
    day.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))

  private def toSmithyDate(day: JavaLocalDate): LocalDate =
    LocalDate(day.getYear, day.getMonthValue, day.getDayOfMonth)
end BillingDashboard
