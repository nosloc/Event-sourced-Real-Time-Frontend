package prototype.server.core.dashboards

import prototype.api.*
import prototype.server.core.dashboards.DashboardFixtures.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The activity figures for a week or a month: which appointments fall in the
  * period, who is busiest, and the evolution over the last periods.
  */
class ActivityDashboardSpec extends AnyFlatSpec with Matchers:

  private val therapists = List(therapist("arb"), therapist("mdp"), therapist("tst"))

  private def week(day: String, appointments: List[Appointment]) =
    ActivityDashboard(therapists, appointments, PeriodUnit.WEEK, date(day))

  private def month(day: String, appointments: List[Appointment]) =
    ActivityDashboard(therapists, appointments, PeriodUnit.MONTH, date(day))

  "A week" should "run from Monday to Sunday around the given day" in {
    // 2026-10-14 is a Wednesday
    val result = week("2026-10-14", Nil)

    result.periodStart shouldBe date("2026-10-12")
    result.periodEnd shouldBe date("2026-10-18")
  }

  it should "be the same week for the Monday and for the Sunday" in {
    week("2026-10-12", Nil).periodStart shouldBe date("2026-10-12")
    week("2026-10-18", Nil).periodStart shouldBe date("2026-10-12")
  }

  it should "count the appointments of the week only, edges included" in {
    val appointments = List(
      appointment("2026-10-11", hour = 23, minute = 30), // Sunday before
      appointment("2026-10-12", hour = 0, minute = 30), // Monday, first
      appointment("2026-10-18", hour = 23, minute = 30), // Sunday, last
      appointment("2026-10-19", hour = 0, minute = 30) // Monday after
    )

    week("2026-10-14", appointments).appointments shouldBe 2
  }

  it should "count the distinct patients seen" in {
    val appointments = List(
      appointment("2026-10-13", patients = List(1, 2)),
      appointment("2026-10-14", patients = List(2, 3))
    )

    week("2026-10-14", appointments).patientsSeen shouldBe 3
  }

  "A month" should "run from its first to its last day" in {
    val october = month("2026-10-14", Nil)
    october.periodStart shouldBe date("2026-10-01")
    october.periodEnd shouldBe date("2026-10-31")

    val february = month("2026-02-10", Nil)
    february.periodEnd shouldBe date("2026-02-28")
  }

  it should "count the appointments of the month only" in {
    val appointments = List(
      appointment("2026-09-30"),
      appointment("2026-10-01"),
      appointment("2026-10-31"),
      appointment("2026-11-01")
    )

    month("2026-10-14", appointments).appointments shouldBe 2
  }

  "The per-therapist counts" should "list every therapist of the company, 0 included" in {
    val appointments = List(
      appointment("2026-10-13", List("arb")),
      appointment("2026-10-14", List("arb", "mdp"))
    )

    week("2026-10-14", appointments).perTherapist shouldBe List(
      TherapistCount(TherapistId("arb"), 2),
      TherapistCount(TherapistId("mdp"), 1),
      TherapistCount(TherapistId("tst"), 0)
    )
  }

  "The busiest therapist" should "be the one with the most appointments" in {
    val appointments = List(
      appointment("2026-10-13", List("mdp")),
      appointment("2026-10-14", List("mdp")),
      appointment("2026-10-15", List("arb"))
    )

    week("2026-10-14", appointments).busiestTherapist shouldBe
      Some(TherapistCount(TherapistId("mdp"), 2))
  }

  it should "be the first by trigram on a tie" in {
    val appointments = List(
      appointment("2026-10-13", List("tst")),
      appointment("2026-10-14", List("arb"))
    )

    week("2026-10-14", appointments).busiestTherapist shouldBe
      Some(TherapistCount(TherapistId("arb"), 1))
  }

  it should "be absent when nobody has an appointment in the period" in {
    val appointments = List(appointment("2026-09-01"))

    week("2026-10-14", appointments).busiestTherapist shouldBe None
  }

  "The evolution" should "list the chosen period and the 5 before it, oldest first" in {
    val result = week("2026-10-14", Nil)

    result.evolution.map(_.start) shouldBe List(
      date("2026-09-07"),
      date("2026-09-14"),
      date("2026-09-21"),
      date("2026-09-28"),
      date("2026-10-05"),
      date("2026-10-12")
    )
  }

  it should "count each period's appointments, with no future period" in {
    val appointments = List(
      appointment("2026-10-13"),
      appointment("2026-10-14"),
      appointment("2026-10-06"),
      appointment("2026-09-08"),
      appointment("2026-10-20") // next week: not shown
    )

    week("2026-10-14", appointments).evolution.map(_.appointments) shouldBe
      List(1, 0, 0, 0, 1, 2)
  }

  it should "go back month by month across a year boundary" in {
    val result = month("2026-02-10", Nil)

    result.evolution.map(_.start) shouldBe List(
      date("2025-09-01"),
      date("2025-10-01"),
      date("2025-11-01"),
      date("2025-12-01"),
      date("2026-01-01"),
      date("2026-02-01")
    )
  }

  "A day's week" should "be cut in Zurich time, not in UTC" in {
    // Monday 2026-10-12 00:30 in Zurich is still Sunday 22:30 in UTC
    val appointments = List(appointment("2026-10-12", hour = 0, minute = 30))

    week("2026-10-14", appointments).appointments shouldBe 1
    week("2026-10-11", appointments).appointments shouldBe 0
  }
