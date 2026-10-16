package prototype.server.core.dashboards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.dashboards.DashboardFixtures.*

/** The directory figures of a company: pure counting over its therapists and
  * appointments, with the clock passed in.
  */
class DirectoryDashboardSpec extends AnyFlatSpec with Matchers:

  private val now = instant("2026-10-16")
  private val therapists = List(
    therapist("arb"),
    therapist("mdp", Role.NEUROPSYCHOLOGIST),
    therapist("tst")
  )

  "DirectoryDashboard" should "count therapists, appointments and distinct patients" in {
    val appointments = List(
      appointment("2026-10-01", List("arb"), List(1, 2)),
      appointment("2026-10-02", List("mdp"), List(2, 3))
    )

    val result = DirectoryDashboard(therapists, appointments, now)

    result.therapists shouldBe 3
    result.appointments shouldBe 2
    result.patientsSeen shouldBe 3
  }

  it should "count the therapists per role" in {
    val result = DirectoryDashboard(therapists, Nil, now)

    result.therapistsPerRole.toSet shouldBe Set(
      RoleCount(Role.PSYCHOLOGIST, 2),
      RoleCount(Role.NEUROPSYCHOLOGIST, 1)
    )
  }

  it should "count the distinct patients of each therapist, 0 for one with no appointment" in {
    val appointments = List(
      appointment("2026-10-01", List("arb"), List(1, 2)),
      appointment("2026-10-02", List("arb"), List(2)),
      appointment("2026-10-03", List("arb", "mdp"), List(3))
    )

    val result = DirectoryDashboard(therapists, appointments, now)

    result.patientsPerTherapist shouldBe List(
      TherapistCount(TherapistId("arb"), 3),
      TherapistCount(TherapistId("mdp"), 1),
      TherapistCount(TherapistId("tst"), 0)
    )
  }

  it should "ignore therapists of other companies in patients per therapist" in {
    val appointments = List(appointment("2026-10-01", List("arb", "out"), List(1)))

    val result = DirectoryDashboard(therapists, appointments, now)

    result.patientsPerTherapist.map(_.therapistId.value) shouldBe List("arb", "mdp", "tst")
  }

  "Active patients" should "include a patient with an appointment in the future" in {
    val appointments = List(appointment("2026-10-20", patients = List(1)))

    DirectoryDashboard(therapists, appointments, now).activePatients shouldBe 1
  }

  it should "include one whose last appointment ended less than 7 days ago" in {
    // Ended on the 10th at 11:00, now is the 16th at 12:00: 6 days and 1 hour
    val appointments = List(appointment("2026-10-10", patients = List(1)))

    DirectoryDashboard(therapists, appointments, now).activePatients shouldBe 1
  }

  it should "leave out one whose last appointment ended more than 7 days ago" in {
    // Ended on the 9th at 11:00: 7 days and 1 hour before now
    val appointments = List(appointment("2026-10-09", patients = List(1)))

    DirectoryDashboard(therapists, appointments, now).activePatients shouldBe 0
  }

  it should "count a patient once, whatever the number of appointments" in {
    val appointments = List(
      appointment("2026-10-14", patients = List(1)),
      appointment("2026-10-20", patients = List(1))
    )

    DirectoryDashboard(therapists, appointments, now).activePatients shouldBe 1
  }

  it should "still count a patient seen long ago as seen, but not active" in {
    val appointments = List(appointment("2026-01-05", patients = List(1)))

    val result = DirectoryDashboard(therapists, appointments, now)

    result.patientsSeen shouldBe 1
    result.activePatients shouldBe 0
  }

  it should "give zeros for a company with no therapist and no appointment" in {
    val result = DirectoryDashboard(Nil, Nil, now)

    result.therapists shouldBe 0
    result.appointments shouldBe 0
    result.patientsSeen shouldBe 0
    result.activePatients shouldBe 0
    result.therapistsPerRole shouldBe Nil
    result.patientsPerTherapist shouldBe Nil
  }
