package prototype.server.core.appointment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.PrestationStatus
import prototype.api.PrestationStatus.*
import prototype.server.core.appointment.appointmentModel.verifyStatusChange

/** The status of a prestation moves forward only: TO_BILL can become BILLED or
  * CANCELLED, BILLED can become PAID or CANCELLED, and PAID and CANCELLED are
  * final. A pure rule, so a table of every pair says it best.
  */
class PrestationStatusSpec extends AnyFlatSpec with Matchers:

  private val allowed: Set[(PrestationStatus, PrestationStatus)] = Set(
    (TO_BILL, BILLED),
    (TO_BILL, CANCELLED),
    (BILLED, PAID),
    (BILLED, CANCELLED)
  )

  private val all = List(TO_BILL, BILLED, PAID, CANCELLED)

  "verifyStatusChange" should "allow TO_BILL to become BILLED or CANCELLED" in {
    verifyStatusChange(TO_BILL, BILLED) shouldBe true
    verifyStatusChange(TO_BILL, CANCELLED) shouldBe true
  }

  it should "allow BILLED to become PAID or CANCELLED" in {
    verifyStatusChange(BILLED, PAID) shouldBe true
    verifyStatusChange(BILLED, CANCELLED) shouldBe true
  }

  it should "not let TO_BILL jump to PAID" in {
    verifyStatusChange(TO_BILL, PAID) shouldBe false
  }

  it should "not go back to an earlier status" in {
    verifyStatusChange(BILLED, TO_BILL) shouldBe false
    verifyStatusChange(PAID, BILLED) shouldBe false
    verifyStatusChange(PAID, TO_BILL) shouldBe false
  }

  it should "treat PAID and CANCELLED as final" in {
    for
      from <- List(PAID, CANCELLED)
      to <- all
    do verifyStatusChange(from, to) shouldBe false
  }

  it should "refuse a change to the same status" in {
    for status <- all do verifyStatusChange(status, status) shouldBe false
  }

  it should "allow exactly the four forward changes, out of all sixteen pairs" in {
    val accepted =
      (for
        from <- all
        to <- all
        if verifyStatusChange(from, to)
      yield (from, to)).toSet

    accepted shouldBe allowed
  }
