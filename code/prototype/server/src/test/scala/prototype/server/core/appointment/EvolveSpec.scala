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
  private val therapists = List(TherapistId("tst"), TherapistId("arb"))
  private val patients = List(PatientId(UUID.fromString("0b2f6d5e-5b0a-4a43-9a9e-3a6b0a1c2d3e")))
  private val kind = AppointmentType.INDIVIDUAL

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  private val morning = range(9, 10)
  private val afternoon = range(14, 15)

  private val created = AppointmentCreated(id, therapists, patients, kind, morning)

  private val existing: AppointmentState =
    Some(Appointment(id, therapists, patients, kind, morning))

  "evolve" should "build the appointment, with all its participants and its type, from AppointmentCreated" in {
    evolve(None, created) shouldBe
      Some(Appointment(id, therapists, patients, kind, morning))
  }

  it should "change the time range on AppointmentUpdated and keep the participants" in {
    evolve(existing, AppointmentUpdated(id, kind, afternoon)) shouldBe
      Some(Appointment(id, therapists, patients, kind, afternoon))
  }

  it should "change the type on AppointmentUpdated" in {
    evolve(existing, AppointmentUpdated(id, AppointmentType.GROUP, morning)) shouldBe
      Some(Appointment(id, therapists, patients, AppointmentType.GROUP, morning))
  }

  it should "leave a missing appointment missing on AppointmentUpdated" in {
    // The easy one to forget: there is nothing to update.
    evolve(None, AppointmentUpdated(id, kind, afternoon)) shouldBe None
  }

  it should "remove the appointment on AppointmentDeleted" in {
    evolve(existing, AppointmentDeleted(id)) shouldBe None
  }

  it should "leave a missing appointment missing on AppointmentDeleted" in {
    evolve(None, AppointmentDeleted(id)) shouldBe None
  }

  it should "reach the expected state when a history is replayed" in {
    val history = List(
      created,
      AppointmentUpdated(id, kind, afternoon),
      AppointmentUpdated(id, kind, morning)
    )

    history.foldLeft(Option.empty[Appointment])(evolve) shouldBe existing
  }

  it should "end with no appointment when a history ends with a delete" in {
    val history = List(
      created,
      AppointmentUpdated(id, kind, afternoon),
      AppointmentDeleted(id)
    )

    history.foldLeft(Option.empty[Appointment])(evolve) shouldBe None
  }
