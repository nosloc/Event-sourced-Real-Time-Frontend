package prototype.shared.dashboards

import java.time.{Instant, LocalDate as JavaLocalDate}

import prototype.api.*

import smithy4s.time.LocalDate

object BillingDashboard:
  // Weeks and days are cut in UTC, whatever the machine's zone is: no
  // time-zone database is needed in the browser, and a day is always this long
  private val secondsPerDay = 86400L
  private val weekCount = 52
  private val oldestCount = 5

  def apply(
      companyAppointments: List[Appointment],
      now: Instant
  ): BillingFigures =
    BillingFigures(
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
    // Each week is the epoch day of its Monday
    val currentWeek =
      mondayOf(Math.floorDiv(now.getEpochSecond, secondsPerDay))
    val byWeek =
      companyAppointments.groupBy(appt => mondayOf(epochDayOf(appt)))
    (weekCount - 1 to 0 by -1).toList.map { weeksAgo =>
      val week = currentWeek - 7L * weeksAgo
      val prestations = byWeek.getOrElse(week, Nil).flatMap(_.prestations)
      WeeklyAmount(
        weekStart = toSmithyDate(JavaLocalDate.ofEpochDay(week)),
        toBill = amountOf(prestations, PrestationStatus.TO_BILL),
        billed = amountOf(prestations, PrestationStatus.BILLED),
        paid = amountOf(prestations, PrestationStatus.PAID)
      )
    }
  end getWeeklyAmounts

  // The day an appointment starts, as an epoch day (days since 1970-01-01).
  private def epochDayOf(appointment: Appointment): Long =
    Math.floorDiv(appointment.timeRange.start.epochSecond, secondsPerDay)

  private def dayOf(appointment: Appointment): JavaLocalDate =
    JavaLocalDate.ofEpochDay(epochDayOf(appointment))

  // The Monday of an epoch day's week, as an epoch day. Day 0 (1970-01-01) is
  // a Thursday, 3 days after a Monday; `floorMod` stays in 0 to 6 before 1970
  private def mondayOf(epochDay: Long): Long =
    epochDay - Math.floorMod(epochDay + 3, 7L)

  private def toSmithyDate(day: JavaLocalDate): LocalDate =
    LocalDate(day.getYear, day.getMonthValue, day.getDayOfMonth)
end BillingDashboard
