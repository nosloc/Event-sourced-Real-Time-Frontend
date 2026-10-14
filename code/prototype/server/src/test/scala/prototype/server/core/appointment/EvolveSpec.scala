package prototype.server.core.appointment

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.appointment.appointmentModel.*
import smithy4s.time.Timestamp

/** `evolve` applies one event to a state. It never rejects anything: the
  * decision was already made. It is the replay logic, shared by the decider and
  * the viewer, so its edge cases matter.
  */
class EvolveSpec extends AnyFlatSpec with Matchers:

  private val id = AppointmentId(UUID.fromString("9fc986ad-fbf1-40f3-9468-a44d1a5fb177"))
  private val therapist = TherapistId("tst")
  private val otherTherapist = TherapistId("arb")

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  private val morning = range(9, 10)
  private val afternoon = range(14, 15)

  private val existing: AppointmentState =
    Some(Appointment(id, therapist, morning))

  "evolve" should "build the appointment from AppointmentCreated" in {
    val state =
      evolve(None, AppointmentCreated(id, therapist, morning))

    state shouldBe Some(Appointment(id, therapist, morning))
  }

  it should "change the time range and the therapist on AppointmentUpdated" in {
    val state =
      evolve(existing, AppointmentUpdated(id, afternoon, otherTherapist))

    state shouldBe Some(Appointment(id, otherTherapist, afternoon))
  }

  it should "keep the appointment id on AppointmentUpdated" in {
    val state = evolve(existing, AppointmentUpdated(id, afternoon, therapist))

    state.map(_.appointmentId) shouldBe Some(id)
  }

  it should "leave a missing appointment missing on AppointmentUpdated" in {
    // The easy one to forget: there is nothing to update.
    evolve(None, AppointmentUpdated(id, afternoon, therapist)) shouldBe None
  }

  it should "remove the appointment on AppointmentDeleted" in {
    evolve(existing, AppointmentDeleted(id)) shouldBe None
  }

  it should "leave a missing appointment missing on AppointmentDeleted" in {
    evolve(None, AppointmentDeleted(id)) shouldBe None
  }

  it should "reach the expected state when a history is replayed" in {
    val history = List(
      AppointmentCreated(id, therapist, morning),
      AppointmentUpdated(id, afternoon, therapist),
      AppointmentUpdated(id, morning, otherTherapist)
    )

    val stepByStep = history.foldLeft(Option.empty[Appointment]) {
      (state, event) => evolve(state, event)
    }

    stepByStep shouldBe Some(Appointment(id, otherTherapist, morning))
  }

  it should "end with no appointment when a history ends with a delete" in {
    val history = List(
      AppointmentCreated(id, therapist, morning),
      AppointmentUpdated(id, afternoon, therapist),
      AppointmentDeleted(id)
    )

    history.foldLeft(Option.empty[Appointment])(evolve) shouldBe None
  }
