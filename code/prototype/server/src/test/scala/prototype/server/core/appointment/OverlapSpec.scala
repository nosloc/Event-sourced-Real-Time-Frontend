package prototype.server.core.appointment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.TimeRange
import prototype.server.core.appointment.appointmentModel.*
import smithy4s.time.Timestamp

/** Tests of the two pure rules on time ranges. Pure functions need no setup, so
  * each test is: build the input, call the function, check the result.
  *
  * Style used here: a test is `"subject" should "behavior" in { ... }`, one
  * rule per test, and the name says the rule so a failure explains itself.
  */
class OverlapSpec extends AnyFlatSpec with Matchers:

  // A range from hour `from` to hour `to` of the same day: `range(9, 11)` is
  // 09:00 to 11:00. Small helpers keep each test to one readable line.
  private def range(from: Int, to: Int): TimeRange =
    TimeRange(at(from), at(to))

  private def at(hour: Int): Timestamp =
    Timestamp.fromEpochSecond(hour * 3600L)

  // --- overlaps -----------------------------------------------------------

  "overlaps" should "be true for identical ranges" in {
    overlaps(range(9, 10), range(9, 10)) shouldBe true
  }

  it should "be true when one range is inside the other" in {
    overlaps(range(9, 12), range(10, 11)) shouldBe true
    overlaps(range(10, 11), range(9, 12)) shouldBe true
  }

  it should "be true when the ranges share only their start" in {
    overlaps(range(9, 11), range(9, 10)) shouldBe true
  }

  it should "be true for a partial overlap, whichever range starts first" in {
    overlaps(range(9, 11), range(10, 12)) shouldBe true
    overlaps(range(10, 12), range(9, 11)) shouldBe true
  }

  it should "be false for back-to-back ranges (one ends when the next starts)" in {
    // 09:00-10:00 then 10:00-11:00: the therapist is free at 10:00 sharp,
    // so these must not count as overlapping. Check both orders.
    overlaps(range(9, 10), range(10, 11)) shouldBe false
    overlaps(range(10, 11), range(9, 10)) shouldBe false
  }

  it should "be false for ranges with a gap between them" in {
    overlaps(range(9, 10), range(11, 12)) shouldBe false
    overlaps(range(11, 12), range(9, 10)) shouldBe false
  }

  it should "give the same answer whichever range comes first (symmetry)" in {
    val ranges = List(
      range(9, 10),
      range(9, 12),
      range(10, 11),
      range(10, 12),
      range(11, 12),
      range(13, 14)
    )
    // Compare every pair of ranges in both orders
    for
      a <- ranges
      b <- ranges
    do overlaps(a, b) shouldBe overlaps(b, a)
  }

  // --- isValidRange -------------------------------------------------------

  "isValidRange" should "accept a range whose end is after its start" in {
    isValidRange(range(9, 10)) shouldBe true
  }

  it should "reject a range whose end equals its start" in {
    isValidRange(range(9, 9)) shouldBe false
  }

  it should "reject a range whose end is before its start" in {
    isValidRange(range(10, 9)) shouldBe false
  }
