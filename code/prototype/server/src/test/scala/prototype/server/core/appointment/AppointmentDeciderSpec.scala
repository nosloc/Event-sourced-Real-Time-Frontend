package prototype.server.core.appointment

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.appointment.appointmentModel.*
import smithy4s.time.Timestamp

/** `decide` turns a command into events (or a rejection) given the current
  * state. The matrix to cover is every command on both states: no appointment
  * yet (`None`) and an existing one (`Some`), plus the invalid range.
  */
class AppointmentDeciderSpec extends AnyFlatSpec with Matchers:

  private val decider = AppointmentDecider()

  private val id = AppointmentId(UUID.fromString("9fc986ad-fbf1-40f3-9468-a44d1a5fb177"))
  private val therapist = TherapistId("tst")

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  private val morning = range(9, 10)
  private val afternoon = range(14, 15)

  private val existing: AppointmentState =
    Some(Appointment(id, therapist, morning))

  private val noAppointment: AppointmentState = None

  // --- create -------------------------------------------------------------

  "decide, for a create command," should "produce AppointmentCreated when the appointment does not exist" in {
    val result =
      decider.decide(noAppointment, CreateAppointmentCommand(id, therapist, morning))

    result shouldBe Right(List(AppointmentCreated(id, therapist, morning)))
  }

  it should "reject it with ALREADY_EXISTS when the appointment exists" in {
    val result =
      decider.decide(existing, CreateAppointmentCommand(id, therapist, afternoon))

    result shouldBe Left(ALREADY_EXISTS)
  }

  it should "reject a range whose end equals its start" in {
    val result =
      decider.decide(noAppointment, CreateAppointmentCommand(id, therapist, range(9, 9)))

    result shouldBe Left(INVALID_TIME_RANGE)
  }

  it should "reject a range whose end is before its start" in {
    val result =
      decider.decide(noAppointment, CreateAppointmentCommand(id, therapist, range(10, 9)))

    result shouldBe Left(INVALID_TIME_RANGE)
  }

  it should "check the range before checking whether the appointment exists" in {
    // Documents the current order: an invalid range on an existing appointment
    // reports the range, not ALREADY_EXISTS.
    val result =
      decider.decide(existing, CreateAppointmentCommand(id, therapist, range(10, 9)))

    result shouldBe Left(INVALID_TIME_RANGE)
  }

  // --- update -------------------------------------------------------------

  "decide, for an update command," should "produce AppointmentUpdated when the appointment exists" in {
    val result =
      decider.decide(existing, UpdateAppointmentCommand(id, afternoon, therapist))

    result shouldBe Right(List(AppointmentUpdated(id, afternoon, therapist)))
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    val result =
      decider.decide(noAppointment, UpdateAppointmentCommand(id, afternoon, therapist))

    result shouldBe Left(NOT_FOUND)
  }

  it should "reject an invalid range" in {
    val result =
      decider.decide(existing, UpdateAppointmentCommand(id, range(15, 14), therapist))

    result shouldBe Left(INVALID_TIME_RANGE)
  }

  // --- delete -------------------------------------------------------------

  "decide, for a delete command," should "produce AppointmentDeleted when the appointment exists" in {
    val result = decider.decide(existing, DeleteAppointmentCommand(id))

    result shouldBe Right(List(AppointmentDeleted(id)))
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    val result = decider.decide(noAppointment, DeleteAppointmentCommand(id))

    result shouldBe Left(NOT_FOUND)
  }

  // --- decide and evolve together ------------------------------------------

  "the events produced by decide" should "build the expected state when applied with evolve" in {
    // Create, then update, replaying each decision's events into the state:
    // this is exactly what the command handler does.
    val afterCreate = decider
      .decide(noAppointment, CreateAppointmentCommand(id, therapist, morning))
      .map(_.foldLeft(noAppointment)(decider.evolve))

    afterCreate shouldBe Right(Some(Appointment(id, therapist, morning)))

    val afterUpdate = afterCreate.flatMap(state =>
      decider
        .decide(state, UpdateAppointmentCommand(id, afternoon, therapist))
        .map(_.foldLeft(state)(decider.evolve))
    )

    afterUpdate shouldBe Right(Some(Appointment(id, therapist, afternoon)))
  }

  "the decider" should "start from no appointment" in {
    decider.initialState shouldBe None
  }
