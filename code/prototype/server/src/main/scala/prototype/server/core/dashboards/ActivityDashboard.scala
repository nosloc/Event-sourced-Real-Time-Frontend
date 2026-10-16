package prototype.server.core.dashboards

import prototype.api.*
import smithy4s.time.LocalDate
import java.time.{DayOfWeek, Instant, ZoneId, LocalDate as JavaLocalDate}
import java.time.temporal.TemporalAdjusters

object ActivityDashboard:
  // Weeks, months and days are cut in one fixed zone, like in the billing
  private val zone = ZoneId.of("Europe/Zurich")
  // The chosen period and the ones before it, in the evolution
  private val periodsBefore = 5

  // An appointment belongs to the period of the day it starts
  private final case class Period(start: JavaLocalDate, end: JavaLocalDate):
    def contains(day: JavaLocalDate): Boolean =
      !day.isBefore(start) && !day.isAfter(end)

  def apply(
      therapists: List[Therapist],
      companyAppointments: List[Appointment],
      unit: PeriodUnit,
      date: LocalDate
  ): GetActivityDashboardOutput =
    val period = periodOf(unit, date.toJava)
    val inPeriod = appointmentsIn(companyAppointments, period)
    val perTherapist = getPerTherapist(therapists, inPeriod)

    GetActivityDashboardOutput(
      periodStart = toSmithyDate(period.start),
      periodEnd = toSmithyDate(period.end),
      appointments = inPeriod.size,
      patientsSeen = inPeriod.flatMap(_.patientIds).distinct.size,
      busiestTherapist = getBusiestTherapist(perTherapist),
      perTherapist = perTherapist,
      evolution = getEvolution(companyAppointments, unit, period)
    )
  end apply

  // Every therapist of the company, including those with no appointment
  private def getPerTherapist(
      companyTherapists: List[Therapist],
      appointments: List[Appointment]
  ): List[TherapistCount] =
    companyTherapists.map { therapist =>
      TherapistCount(
        therapist.therapistId,
        appointments.count(_.therapistIds.contains(therapist.therapistId))
      )
    }
  end getPerTherapist

  // None when nobody has an appointment; on a tie, the first by trigram
  private def getBusiestTherapist(
      perTherapist: List[TherapistCount]
  ): Option[TherapistCount] =
    perTherapist
      .filter(_.count > 0)
      .sortBy(entry => (-entry.count, entry.therapistId.value))
      .headOption
  end getBusiestTherapist

  // The chosen period and the ones before it, oldest first
  private def getEvolution(
      companyAppointments: List[Appointment],
      unit: PeriodUnit,
      chosen: Period
  ): List[PeriodCount] =
    (periodsBefore to 0 by -1).toList.map { periodsAgo =>
      val period = periodOf(unit, shift(unit, chosen.start, -periodsAgo))
      PeriodCount(
        start = toSmithyDate(period.start),
        appointments = appointmentsIn(companyAppointments, period).size
      )
    }
  end getEvolution

  private def appointmentsIn(
      appointments: List[Appointment],
      period: Period
  ): List[Appointment] =
    appointments.filter(appt => period.contains(dayOf(appt)))

  // The week (Monday to Sunday) or the month that contains the day
  private def periodOf(unit: PeriodUnit, day: JavaLocalDate): Period =
    unit match
      case PeriodUnit.WEEK =>
        val monday = day.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))
        Period(monday, monday.plusDays(6))
      case PeriodUnit.MONTH =>
        Period(
          day.withDayOfMonth(1),
          day.`with`(TemporalAdjusters.lastDayOfMonth())
        )

  private def shift(
      unit: PeriodUnit,
      day: JavaLocalDate,
      periods: Int
  ): JavaLocalDate =
    unit match
      case PeriodUnit.WEEK  => day.plusWeeks(periods.toLong)
      case PeriodUnit.MONTH => day.plusMonths(periods.toLong)

  private def dayOf(appointment: Appointment): JavaLocalDate =
    Instant
      .ofEpochSecond(appointment.timeRange.start.epochSecond)
      .atZone(zone)
      .toLocalDate

  private def toSmithyDate(day: JavaLocalDate): LocalDate =
    LocalDate(day.getYear, day.getMonthValue, day.getDayOfMonth)
end ActivityDashboard
