package prototype.server.core.appointment.commands

import java.util.UUID

import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.core.appointment.domain.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The state the appointment commands rebuild from the history they read: the
  * live appointments, and who is busy in a time range. Pure functions, so no
  * store is involved.
  */
class AppointmentsStateSpec extends AnyFlatSpec with Matchers:

  private val noHistory = AppointmentsState(Set.empty, Set.empty)
  private val patient = UUID.fromString("00000000-0000-0000-0000-0000000000c1")

  private def created(n: Int, therapists: List[String], from: Int, to: Int) =
    AppointmentCreated(
      appointmentId(n),
      therapists,
      List(patient),
      "INDIVIDUAL",
      millis(from),
      millis(to)
    )

  private def busy(state: AppointmentsState, from: Int, to: Int) =
    state.overlappingTherapists(
      List("abc", "xyz"),
      millis(from),
      millis(to),
      ignoring = None
    )

  "An appointment" should "make its therapists and patients busy when the ranges overlap" in {
    val state = noHistory.afterCreated(created(1, List("abc"), 10, 12))
    busy(state, 11, 13) shouldBe Set("abc")
    state.overlappingPatients(
      List(patient),
      millis(11),
      millis(13),
      None
    ) shouldBe
      Set(patient)
  }

  it should "not overlap one that starts when it ends" in {
    busy(
      noHistory.afterCreated(created(1, List("abc"), 10, 11)),
      11,
      12
    ) shouldBe empty
  }

  it should "only name the people asked about" in {
    noHistory
      .afterCreated(created(1, List("abc", "qrs"), 10, 11))
      .overlappingTherapists(
        List("abc"),
        millis(10),
        millis(11),
        None
      ) shouldBe Set(
      "abc"
    )
  }

  it should "be left out when its own id is ignored" in {
    noHistory
      .afterCreated(created(1, List("abc"), 10, 11))
      .overlappingTherapists(
        List("abc"),
        millis(10),
        millis(11),
        ignoring = Some(appointmentId(1))
      ) shouldBe empty
  }

  "afterUpdated" should "take the full new values, even for an appointment never seen" in {
    val state = noHistory.afterUpdated(
      AppointmentUpdated(
        appointmentId(1),
        List("xyz"),
        Nil,
        "COUPLE",
        millis(14),
        millis(15)
      )
    )
    state.appointments(appointmentId(1)) shouldBe
      AppointmentSlot(Set("xyz"), Set.empty, "COUPLE", millis(14), millis(15))
  }

  it should "free someone removed from the appointment" in {
    val state = noHistory
      .afterCreated(created(1, List("abc", "xyz"), 10, 11))
      .afterUpdated(
        AppointmentUpdated(
          appointmentId(1),
          List("xyz"),
          Nil,
          "INDIVIDUAL",
          millis(10),
          millis(11)
        )
      )
    busy(state, 10, 11) shouldBe Set("xyz")
  }

  "afterDeleted" should "free everyone in the appointment" in {
    val state = noHistory
      .afterCreated(created(1, List("abc"), 10, 11))
      .afterDeleted(AppointmentDeleted(appointmentId(1)))
    busy(state, 10, 11) shouldBe empty
    state.exists(appointmentId(1)) shouldBe false
  }
