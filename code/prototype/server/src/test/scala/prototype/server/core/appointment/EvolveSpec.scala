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
    Some(Appointment(id, therapists, patients, kind, morning, Nil))

  "evolve" should "build the appointment, with all its participants and its type, from AppointmentCreated" in {
    evolve(None, created) shouldBe
      Some(Appointment(id, therapists, patients, kind, morning, Nil))
  }

  it should "change the time range on AppointmentUpdated" in {
    evolve(existing, AppointmentUpdated(id, kind, afternoon, therapists, patients)) shouldBe
      Some(Appointment(id, therapists, patients, kind, afternoon, Nil))
  }

  it should "change the type on AppointmentUpdated" in {
    evolve(existing, AppointmentUpdated(id, AppointmentType.GROUP, morning, therapists, patients)) shouldBe
      Some(Appointment(id, therapists, patients, AppointmentType.GROUP, morning, Nil))
  }

  it should "change the participants on AppointmentUpdated" in {
    val otherTherapists = List(TherapistId("lbo"))
    evolve(existing, AppointmentUpdated(id, kind, morning, otherTherapists, Nil)) shouldBe
      Some(Appointment(id, otherTherapists, Nil, kind, morning, Nil))
  }

  it should "leave a missing appointment missing on AppointmentUpdated" in {
    // The easy one to forget: there is nothing to update.
    evolve(None, AppointmentUpdated(id, kind, afternoon, therapists, patients)) shouldBe None
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
      AppointmentUpdated(id, kind, afternoon, therapists, patients),
      AppointmentUpdated(id, kind, morning, therapists, patients)
    )

    history.foldLeft(Option.empty[Appointment])(evolve) shouldBe existing
  }

  it should "end with no appointment when a history ends with a delete" in {
    val history = List(
      created,
      AppointmentUpdated(id, kind, afternoon, therapists, patients),
      AppointmentDeleted(id)
    )

    history.foldLeft(Option.empty[Appointment])(evolve) shouldBe None
  }

  // --- prestations --------------------------------------------------------

  private val prestationId =
    PrestationId(UUID.fromString("5a3c9a7e-0d1f-4c58-8f0e-6c0a3d9b2a11"))
  private val otherPrestationId =
    PrestationId(UUID.fromString("7b1d2c3e-4f50-4a61-9b72-8c93a4b5c6d7"))

  private val firstCreated =
    PrestationCreated(id, prestationId, "Consultation", 120.0f, 1)
  private val secondCreated =
    PrestationCreated(id, otherPrestationId, "Report", 45.5f, 2)

  private def prestationsOf(state: AppointmentState): List[Prestation] =
    state.map(_.prestations).getOrElse(Nil)

  it should "add a prestation in status TO_BILL on PrestationCreated" in {
    evolve(existing, firstCreated) shouldBe
      existing.map(
        _.copy(prestations =
          List(
            Prestation(id, prestationId, "Consultation", 1, 120.0f, PrestationStatus.TO_BILL)
          )
        )
      )
  }

  it should "keep the order of the prestations as they were added" in {
    val state = List(firstCreated, secondCreated).foldLeft(existing)(evolve)

    prestationsOf(state).map(_.prestationId) shouldBe
      List(prestationId, otherPrestationId)
  }

  it should "leave a missing appointment missing on PrestationCreated" in {
    evolve(None, firstCreated) shouldBe None
  }

  it should "change only the status of the named prestation on PrestationStatusUpdated" in {
    val twoPrestations = List(firstCreated, secondCreated).foldLeft(existing)(evolve)

    val state = evolve(
      twoPrestations,
      PrestationStatusUpdated(id, prestationId, PrestationStatus.BILLED)
    )

    prestationsOf(state).map(p => (p.prestationId, p.status)) shouldBe
      List(
        (prestationId, PrestationStatus.BILLED),
        (otherPrestationId, PrestationStatus.TO_BILL)
      )
  }

  it should "remove only the named prestation on PrestationDeleted" in {
    val twoPrestations = List(firstCreated, secondCreated).foldLeft(existing)(evolve)

    val state = evolve(twoPrestations, PrestationDeleted(id, prestationId))

    prestationsOf(state).map(_.prestationId) shouldBe List(otherPrestationId)
  }

  it should "keep the prestations when the appointment is updated" in {
    val state = List(
      firstCreated,
      AppointmentUpdated(id, AppointmentType.GROUP, afternoon, therapists, patients)
    ).foldLeft(existing)(evolve)

    prestationsOf(state).map(_.prestationId) shouldBe List(prestationId)
    state.map(_.timeRange) shouldBe Some(afternoon)
  }

  it should "reach the expected prestations when a history is replayed" in {
    val history = List(
      created,
      firstCreated,
      secondCreated,
      PrestationStatusUpdated(id, prestationId, PrestationStatus.BILLED),
      PrestationStatusUpdated(id, prestationId, PrestationStatus.PAID),
      PrestationDeleted(id, otherPrestationId)
    )

    val state = history.foldLeft(Option.empty[Appointment])(evolve)

    prestationsOf(state).map(p => (p.prestationId, p.status)) shouldBe
      List((prestationId, PrestationStatus.PAID))
  }
