package prototype.server.core.dashboards

import java.time.{Instant, LocalDate as JavaLocalDate, ZoneId}
import java.util.UUID

import prototype.api.*

import smithy4s.time.{LocalDate, Timestamp}

/** Small builders for the dashboard specs. Days are written as "2026-10-12"
  * and read in Zurich time, like the dashboards do.
  */
object DashboardFixtures:
  private val zone = ZoneId.of("Europe/Zurich")

  val company: CompanyId = CompanyId(UUID.fromString("00000000-0000-4000-8000-0000000000c1"))

  def therapist(trigram: String, role: Role = Role.PSYCHOLOGIST): Therapist =
    Therapist("First", "Last", TherapistId(trigram), company, role)

  def patient(n: Int): PatientId =
    PatientId(UUID.fromString(f"00000000-0000-4000-8000-$n%012d"))

  def date(day: String): LocalDate =
    val d = JavaLocalDate.parse(day)
    LocalDate(d.getYear, d.getMonthValue, d.getDayOfMonth)

  def timestamp(day: String, hour: Int, minute: Int = 0): Timestamp =
    Timestamp.fromEpochSecond(
      JavaLocalDate.parse(day).atTime(hour, minute).atZone(zone).toEpochSecond
    )

  def instant(day: String, hour: Int = 12): Instant =
    Instant.ofEpochSecond(timestamp(day, hour).epochSecond)

  def prestation(
      status: PrestationStatus,
      quantity: Int = 1,
      unitPrice: Float = 100f
  ): Prestation =
    Prestation(
      AppointmentId(UUID.randomUUID()),
      PrestationId(UUID.randomUUID()),
      "Consultation",
      quantity,
      unitPrice,
      status
    )

  /** An appointment of one hour starting on `day` at `hour`. */
  def appointment(
      day: String,
      therapists: List[String] = List("arb"),
      patients: List[Int] = List(1),
      hour: Int = 10,
      minute: Int = 0,
      prestations: List[Prestation] = Nil,
      kind: AppointmentType = AppointmentType.INDIVIDUAL
  ): Appointment =
    val start = timestamp(day, hour, minute)
    Appointment(
      AppointmentId(UUID.randomUUID()),
      therapists.map(TherapistId(_)),
      patients.map(patient),
      kind,
      TimeRange(start, Timestamp.fromEpochSecond(start.epochSecond + 3600)),
      prestations
    )
end DashboardFixtures
