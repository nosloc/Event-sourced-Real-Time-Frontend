package prototype.server.core.appointment

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.appointment.appointmentModel.*
import prototype.StatusTransitions.verifyStatusChange
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
    Some(Appointment(id, therapists, patients, kind, morning, Nil))

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
    decider.decide(existing, UpdateAppointmentCommand(id, kind, afternoon, therapists, patients)) shouldBe
      Right(List(AppointmentUpdated(id, kind, afternoon, therapists, patients)))
  }

  it should "carry the chosen type, which may differ from the current one" in {
    decider.decide(
      existing,
      UpdateAppointmentCommand(id, AppointmentType.FAMILY, afternoon, therapists, patients)
    ) shouldBe Right(
      List(AppointmentUpdated(id, AppointmentType.FAMILY, afternoon, therapists, patients))
    )
  }

  it should "carry the new participants, which may differ from the current ones" in {
    val otherTherapists = List(TherapistId("lbo"))
    val noPatients = Nil
    decider.decide(
      existing,
      UpdateAppointmentCommand(id, kind, morning, otherTherapists, noPatients)
    ) shouldBe Right(
      List(AppointmentUpdated(id, kind, morning, otherTherapists, noPatients))
    )
  }

  it should "reject it with NO_THERAPIST when there is no therapist" in {
    decider.decide(
      existing,
      UpdateAppointmentCommand(id, kind, afternoon, Nil, patients)
    ) shouldBe Left(NO_THERAPIST)
  }

  it should "accept an update with no patient" in {
    decider.decide(
      existing,
      UpdateAppointmentCommand(id, kind, afternoon, therapists, Nil)
    ) shouldBe Right(
      List(AppointmentUpdated(id, kind, afternoon, therapists, Nil))
    )
  }

  it should "check the therapists before the appointment exists" in {
    // Documents the current order, as for create: the participants first
    decider.decide(
      noAppointment,
      UpdateAppointmentCommand(id, kind, afternoon, Nil, patients)
    ) shouldBe Left(NO_THERAPIST)
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    decider.decide(noAppointment, UpdateAppointmentCommand(id, kind, afternoon, therapists, patients)) shouldBe
      Left(NOT_FOUND)
  }

  it should "reject an invalid range" in {
    decider.decide(existing, UpdateAppointmentCommand(id, kind, range(15, 14), therapists, patients)) shouldBe
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
      Some(Appointment(id, therapists, patients, kind, morning, Nil))
    )

    val afterUpdate = afterCreate.flatMap(state =>
      decider
        .decide(state, UpdateAppointmentCommand(id, kind, afternoon, therapists, patients))
        .map(_.foldLeft(state)(decider.evolve))
    )

    afterUpdate shouldBe Right(
      Some(Appointment(id, therapists, patients, kind, afternoon, Nil))
    )
  }

  "the decider" should "start from no appointment" in {
    decider.initialState shouldBe None
  }

  // --- prestations --------------------------------------------------------

  private val prestationId =
    PrestationId(UUID.fromString("5a3c9a7e-0d1f-4c58-8f0e-6c0a3d9b2a11"))
  private val otherPrestationId =
    PrestationId(UUID.fromString("7b1d2c3e-4f50-4a61-9b72-8c93a4b5c6d7"))

  private def prestation(
      prestationId: PrestationId,
      status: PrestationStatus
  ): Prestation =
    Prestation(id, prestationId, "Consultation", 1, 120.0f, status)

  // The existing appointment, holding one prestation in the given status
  private def withPrestation(status: PrestationStatus): AppointmentState =
    existing.map(_.copy(prestations = List(prestation(prestationId, status))))

  private def addPrestation(
      label: String = "Consultation",
      quantity: Int = 1,
      unitPrice: Float = 120.0f,
      state: AppointmentState = existing
  ) =
    decider.decide(
      state,
      CreatePrestationCommand(id, prestationId, label, unitPrice, quantity)
    )

  "decide, for a create prestation command," should "produce PrestationCreated when the appointment exists" in {
    addPrestation(label = "Consultation", quantity = 2, unitPrice = 85.5f) shouldBe
      Right(List(PrestationCreated(id, prestationId, "Consultation", 85.5f, 2)))
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    addPrestation(state = noAppointment) shouldBe Left(NOT_FOUND)
  }

  it should "accept a price of zero" in {
    addPrestation(unitPrice = 0.0f).isRight shouldBe true
  }

  it should "reject an empty label" in {
    addPrestation(label = "") shouldBe Left(INVALID_PRESTATION)
  }

  it should "reject a label of spaces only" in {
    addPrestation(label = "   ") shouldBe Left(INVALID_PRESTATION)
  }

  it should "reject a quantity of zero" in {
    addPrestation(quantity = 0) shouldBe Left(INVALID_PRESTATION)
  }

  it should "reject a negative quantity" in {
    addPrestation(quantity = -1) shouldBe Left(INVALID_PRESTATION)
  }

  it should "reject a negative price" in {
    addPrestation(unitPrice = -0.5f) shouldBe Left(INVALID_PRESTATION)
  }

  it should "reject a price that is not a number" in {
    addPrestation(unitPrice = Float.NaN) shouldBe Left(INVALID_PRESTATION)
  }

  it should "reject an infinite price" in {
    addPrestation(unitPrice = Float.PositiveInfinity) shouldBe
      Left(INVALID_PRESTATION)
  }

  "decide, for an update prestation status command," should "produce PrestationStatusUpdated for an allowed change" in {
    decider.decide(
      withPrestation(PrestationStatus.TO_BILL),
      UpdatePrestationStatusCommand(id, prestationId, PrestationStatus.BILLED)
    ) shouldBe Right(
      List(PrestationStatusUpdated(id, prestationId, PrestationStatus.BILLED))
    )
  }

  it should "reject a change that is not allowed with INVALID_STATUS_CHANGE" in {
    decider.decide(
      withPrestation(PrestationStatus.TO_BILL),
      UpdatePrestationStatusCommand(id, prestationId, PrestationStatus.PAID)
    ) shouldBe Left(INVALID_STATUS_CHANGE)
  }

  it should "reject any change from a final status" in {
    for
      from <- List(PrestationStatus.PAID, PrestationStatus.CANCELLED)
      to <- PrestationStatus.values
    do
      decider.decide(
        withPrestation(from),
        UpdatePrestationStatusCommand(id, prestationId, to)
      ) shouldBe Left(INVALID_STATUS_CHANGE)
  }

  it should "agree with verifyStatusChange for every pair of statuses" in {
    for
      from <- PrestationStatus.values
      to <- PrestationStatus.values
    do
      val result = decider.decide(
        withPrestation(from),
        UpdatePrestationStatusCommand(id, prestationId, to)
      )
      result.isRight shouldBe verifyStatusChange(from, to)
  }

  it should "reject it with PRESTATION_NOT_FOUND for an unknown prestation" in {
    decider.decide(
      withPrestation(PrestationStatus.TO_BILL),
      UpdatePrestationStatusCommand(id, otherPrestationId, PrestationStatus.BILLED)
    ) shouldBe Left(PRESTATION_NOT_FOUND)
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    decider.decide(
      noAppointment,
      UpdatePrestationStatusCommand(id, prestationId, PrestationStatus.BILLED)
    ) shouldBe Left(NOT_FOUND)
  }

  "decide, for a delete prestation command," should "produce PrestationDeleted whatever the status" in {
    for status <- PrestationStatus.values do
      decider.decide(
        withPrestation(status),
        DeletePrestationCommand(id, prestationId)
      ) shouldBe Right(List(PrestationDeleted(id, prestationId)))
  }

  it should "reject it with PRESTATION_NOT_FOUND for an unknown prestation" in {
    decider.decide(
      withPrestation(PrestationStatus.TO_BILL),
      DeletePrestationCommand(id, otherPrestationId)
    ) shouldBe Left(PRESTATION_NOT_FOUND)
  }

  it should "reject it with NOT_FOUND when the appointment does not exist" in {
    decider.decide(
      noAppointment,
      DeletePrestationCommand(id, prestationId)
    ) shouldBe Left(NOT_FOUND)
  }
