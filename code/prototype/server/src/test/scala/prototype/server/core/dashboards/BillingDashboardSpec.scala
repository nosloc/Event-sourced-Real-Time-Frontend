package prototype.server.core.dashboards

import prototype.api.*
import prototype.api.PrestationStatus.*
import prototype.server.core.dashboards.DashboardFixtures.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The billing figures of a company: amounts per status, the oldest
  * appointments still to bill, and the weekly amounts of the last year.
  */
class BillingDashboardSpec extends AnyFlatSpec with Matchers:

  // A Friday; its week starts on Monday 2026-10-12
  private val now = instant("2026-10-16")

  "The amount per status" should "sum quantity times unit price for each status" in {
    val appointments = List(
      appointment("2026-10-01", prestations = List(prestation(TO_BILL, 2, 50f), prestation(PAID, 1, 200f))),
      appointment("2026-10-02", prestations = List(prestation(TO_BILL, 1, 30f), prestation(BILLED, 3, 10f), prestation(CANCELLED, 1, 80f)))
    )

    BillingDashboard(appointments, now).amountPerStatus shouldBe
      AmountPerStatus(toBill = 130f, billed = 30f, paid = 200f, cancelled = 80f)
  }

  it should "be all zeros without a prestation" in {
    BillingDashboard(List(appointment("2026-10-01")), now).amountPerStatus shouldBe
      AmountPerStatus(0f, 0f, 0f, 0f)
  }

  "The oldest still to bill" should "list the appointments with something to bill, oldest first" in {
    val older = appointment("2026-09-01", prestations = List(prestation(TO_BILL, 1, 100f)))
    val newer = appointment("2026-10-01", prestations = List(prestation(TO_BILL, 1, 40f)))
    val done = appointment("2026-08-01", prestations = List(prestation(PAID)))
    val empty = appointment("2026-07-01")

    val result = BillingDashboard(List(newer, done, empty, older), now).oldestToBill

    result.map(_.appointmentId) shouldBe List(older.appointmentId, newer.appointmentId)
    result.map(_.date) shouldBe List(date("2026-09-01"), date("2026-10-01"))
  }

  it should "show only the amount still to bill of an appointment" in {
    val appt = appointment(
      "2026-10-01",
      prestations = List(prestation(TO_BILL, 2, 25f), prestation(BILLED, 1, 500f))
    )

    BillingDashboard(List(appt), now).oldestToBill.map(_.amount) shouldBe List(50f)
  }

  it should "keep the type and the therapists of the appointment" in {
    val appt = appointment(
      "2026-10-01",
      therapists = List("arb", "mdp"),
      kind = AppointmentType.COUPLE,
      prestations = List(prestation(TO_BILL))
    )

    val item = BillingDashboard(List(appt), now).oldestToBill.head

    item.appointmentType shouldBe AppointmentType.COUPLE
    item.therapistIds shouldBe appt.therapistIds
  }

  it should "stop at 5 appointments" in {
    val appointments = (1 to 7).toList.map(i =>
      appointment(f"2026-09-$i%02d", prestations = List(prestation(TO_BILL)))
    )

    val result = BillingDashboard(appointments, now).oldestToBill

    result should have size 5
    result.map(_.date) shouldBe (1 to 5).toList.map(i => date(f"2026-09-$i%02d"))
  }

  "The weekly amounts" should "cover 52 weeks, oldest first, ending with the current week" in {
    val weekly = BillingDashboard(Nil, now).weekly

    weekly should have size 52
    weekly.last.weekStart shouldBe date("2026-10-12")
    weekly.head.weekStart shouldBe date("2025-10-20")
  }

  it should "put each prestation in the week of its appointment" in {
    val appointments = List(
      appointment("2026-10-13", prestations = List(prestation(TO_BILL, 1, 10f))),
      appointment("2026-10-16", prestations = List(prestation(BILLED, 1, 20f), prestation(PAID, 1, 30f))),
      appointment("2026-10-06", prestations = List(prestation(PAID, 1, 70f)))
    )

    val weekly = BillingDashboard(appointments, now).weekly

    weekly.last shouldBe WeeklyAmount(date("2026-10-12"), toBill = 10f, billed = 20f, paid = 30f)
    weekly(50) shouldBe WeeklyAmount(date("2026-10-05"), toBill = 0f, billed = 0f, paid = 70f)
  }

  it should "leave cancelled prestations out" in {
    val appointments = List(
      appointment("2026-10-13", prestations = List(prestation(CANCELLED, 1, 999f)))
    )

    BillingDashboard(appointments, now).weekly.last shouldBe
      WeeklyAmount(date("2026-10-12"), 0f, 0f, 0f)
  }

  it should "start a week on Monday, in Zurich time" in {
    val appointments = List(
      appointment("2026-10-11", hour = 23, minute = 30, prestations = List(prestation(PAID, 1, 5f))), // Sunday
      appointment("2026-10-12", hour = 0, minute = 30, prestations = List(prestation(PAID, 1, 7f))) // Monday
    )

    val weekly = BillingDashboard(appointments, now).weekly

    weekly(50).paid shouldBe 5f
    weekly(51).paid shouldBe 7f
  }

  it should "not show an appointment older than 52 weeks" in {
    val appointments = List(
      appointment("2025-10-13", prestations = List(prestation(PAID, 1, 100f)))
    )

    BillingDashboard(appointments, now).weekly.map(_.paid).sum shouldBe 0f
  }
