package prototype.server.core.appointment

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.appointment.appointmentModel.*
import smithy4s.time.Timestamp

/** `decide` turns a command into events (or a rejection) given the current
  * state. The matrix to cover is every command on both states: no appointment
  * yet (`None`) and an existing one (`Some`), plus the invalid range and the
  * participants rule (at least one therapist; patients may be absent).
  */
class AppointmentDeciderSpec extends AnyFlatSpec with Matchers:

  private val decider = AppointmentDecider()

  private val id = AppointmentId(UUID.fromString("9fc986ad-fbf1-40f3-9468-a44d1a5fb177"))
  private val therapists = List(TherapistId("tst"), TherapistId("arb"))
  private val patients = List(PatientId(UUID.fromString("0b2f6d5e-5b0a-4a43-9a9e-3a6b0a1c2d3e")))
  private val kind = AppointmentType.COUPLE

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  private val morning = range(9, 10)
  private val afternoon = range(14, 15)

  private def create(
      ths: List[TherapistId] = therapists,
      pts: List[PatientId] = patients,
      r: TimeRange = morning
  ) = CreateAppointmentCommand(id, ths, pts, kind, r)

  private val existing: AppointmentState =
    Some(Appointment(id, therapists, patients, kind, morning))

  private val noAppointment: AppointmentState = None

  // --- create -------------------------------------------------------------

  "decide, for a create command," should "produce AppointmentCreated, with every participant and the type, when the appointment does not exist" in {
    decider.decide(noAppointment, create()) shouldBe
      Right(List(AppointmentCreated(id, therapists, patients, kind, morning)))
  }

  it should "reject it with ALREADY_EXISTS when the appointment exists" in {
    decider.decide(existing, create(r = afternoon)) shouldBe Left(ALREADY_EXISTS)
  }

  it should "reject a range whose end equals its start" in {
    decider.decide(noAppointment, create(r = range(9, 9))) shouldBe
      Left(INVALID_TIME_RANGE)
  }

  it should "reject a range whose end is before its start" in {
    decider.decide(noAppointment, create(r = range(10, 9))) shouldBe
      Left(INVALID_TIME_RANGE)
  }

  it should "check the range before checking whether the appointment exists" in {
    // Documents the current order: an invalid range on an existing appointment
    // reports the range, not ALREADY_EXISTS.
    decider.decide(existing, create(r = range(10, 9))) shouldBe
      Left(INVALID_TIME_RANGE)
  }

  it should "reject it with NO_THERAPIST when there is no therapist" in {
    decider.decide(noAppointment, create(ths = Nil)) shouldBe Left(NO_THERAPIST)
  }

  it should "accept an appointment with no patient" in {
    decider.decide(noAppointment, create(pts = Nil)) shouldBe
      Right(List(AppointmentCreated(id, therapists, Nil, kind, morning)))
  }

  it should "check the therapists before the range" in {
    // Documents the current order: with no therapist and a bad range, the
    // participants are reported.
    decider.decide(noAppointment, create(ths = Nil, r = range(10, 9))) shouldBe
      Left(NO_THERAPIST)
  }

  // --- update -------------------------------------------------------------

  "decide, for an update command," should "produce AppointmentUpdated when the appointment exists" in {
    decider.decide(existing, UpdateAppointmentCommand(id, kind, afternoon)) shouldBe
      Right(List(AppointmentUpdated(id, kind, afternoon)))
  }

  it should "carry the chosen type, which may differ from the current one" in {
    decider.decide(
      existing,
      UpdateAppointmentCommand(id, AppointmentType.FAMILY, afternoon)
    ) shouldBe Right(
      List(AppointmentUpdated(id, AppointmentType.FAMILY, afternoon))
    )
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    decider.decide(noAppointment, UpdateAppointmentCommand(id, kind, afternoon)) shouldBe
      Left(NOT_FOUND)
  }

  it should "reject an invalid range" in {
    decider.decide(existing, UpdateAppointmentCommand(id, kind, range(15, 14))) shouldBe
      Left(INVALID_TIME_RANGE)
  }

  // --- delete -------------------------------------------------------------

  "decide, for a delete command," should "produce AppointmentDeleted when the appointment exists" in {
    decider.decide(existing, DeleteAppointmentCommand(id)) shouldBe
      Right(List(AppointmentDeleted(id)))
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    decider.decide(noAppointment, DeleteAppointmentCommand(id)) shouldBe
      Left(NOT_FOUND)
  }

  // --- decide and evolve together ------------------------------------------

  "the events produced by decide" should "build the expected state when applied with evolve" in {
    // Create, then update, replaying each decision's events into the state:
    // this is exactly what the command handler does.
    val afterCreate = decider
      .decide(noAppointment, create())
      .map(_.foldLeft(noAppointment)(decider.evolve))

    afterCreate shouldBe Right(
      Some(Appointment(id, therapists, patients, kind, morning))
    )

    val afterUpdate = afterCreate.flatMap(state =>
      decider
        .decide(state, UpdateAppointmentCommand(id, kind, afternoon))
        .map(_.foldLeft(state)(decider.evolve))
    )

    afterUpdate shouldBe Right(
      Some(Appointment(id, therapists, patients, kind, afternoon))
    )
  }

  "the decider" should "start from no appointment" in {
    decider.initialState shouldBe None
  }
