package prototype.server

import java.time.OffsetDateTime
import java.util.UUID

import prototype.api.*
import smithy4s.time.Timestamp

/** Fictional dummy data loaded at boot. Ids are defined once and reused, so the
  * lists always reference each other consistently.
  */
object Seed:

  private def uuid(value: String): UUID = UUID.fromString(value)

  // Times are written in Swiss local time (UTC+2 until 25 Oct 2026) and stored as instants.
  private def at(dateTime: String): Timestamp =
    Timestamp.fromOffsetDateTime(OffsetDateTime.parse(dateTime))

  private def range(start: String, end: String): TimeRange =
    TimeRange(at(start), at(end))

  // Companies
  val exampleClinic: Company = Company(
    CompanyId(uuid("13ddd512-9f19-4af3-b5a9-af79bd82411e")),
    "Example Clinic",
    Address("Example Street", "1", "Exampleville", "VD")
  )
  val demoHealthCenter: Company = Company(
    CompanyId(uuid("070874c8-5296-452e-8c91-cfaf54385932")),
    "Demo Health Center",
    Address("Sample Avenue", "2", "Sampletown", "GE")
  )
  val companies: List[Company] = List(exampleClinic, demoHealthCenter)

  // Therapists
  private def therapist(
      trigram: String,
      firstName: String,
      lastName: String,
      company: Company,
      role: Role
  ): Therapist =
    Therapist(
      firstName,
      lastName,
      TherapistId(trigram),
      company.companyId,
      role
    )

  val arb =
    therapist("arb", "Alexandre", "Robert", exampleClinic, Role.PSYCHOLOGIST)
  val mdp = therapist(
    "mdp",
    "Marie",
    "Dupont",
    demoHealthCenter,
    Role.NEUROPSYCHOLOGIST
  )
  val jma =
    therapist("jma", "Jean", "Martin", exampleClinic, Role.GENERAL_PRACTITIONER)
  val sda =
    therapist("sda", "Sophie", "Durand", demoHealthCenter, Role.PSYCHOLOGIST)
  val lbo = therapist("lbo", "Lucas", "Bonvin", exampleClinic, Role.PHYSICIAN)
  val fgr =
    therapist("fgr", "Fanny", "Grandjean", demoHealthCenter, Role.PSYCHOLOGIST)
  val nle =
    therapist("nle", "Nicolas", "Leuba", exampleClinic, Role.NEUROPSYCHOLOGIST)
  val tpe = therapist(
    "tpe",
    "Tania",
    "Perret",
    demoHealthCenter,
    Role.GENERAL_PRACTITIONER
  )

  val therapists: List[Therapist] = List(arb, mdp, jma, sda, lbo, fgr, nle, tpe)

  // Appointments: arb has 3, mdp has 2, jma/sda/lbo have 1 each, fgr/nle/tpe have none
  private def appointment(
      id: String,
      therapist: Therapist,
      timeRange: TimeRange
  ): Appointment =
    Appointment(AppointmentId(uuid(id)), therapist.therapistId, timeRange)

  val appointments: List[Appointment] = List(
    appointment(
      "9fc986ad-fbf1-40f3-9468-a44d1a5fb177",
      arb,
      range("2026-10-12T09:00:00+02:00", "2026-10-12T09:45:00+02:00")
    ),
    appointment(
      "db842a24-833c-40d8-9509-dea565e21576",
      arb,
      range("2026-10-12T14:00:00+02:00", "2026-10-12T15:00:00+02:00")
    ),
    appointment(
      "2b738424-f9ad-4c93-b8da-3d7ead6051a6",
      arb,
      range("2026-10-14T10:30:00+02:00", "2026-10-14T11:15:00+02:00")
    ),
    appointment(
      "51a05d9a-71bd-42fd-a2f9-48c51927cab0",
      mdp,
      range("2026-10-13T08:30:00+02:00", "2026-10-13T09:30:00+02:00")
    ),
    appointment(
      "279eee00-7029-498b-989a-07bbf0f6353e",
      mdp,
      range("2026-10-15T13:00:00+02:00", "2026-10-15T14:30:00+02:00")
    ),
    appointment(
      "c1db1663-b894-4e8f-a97f-da78edce9722",
      jma,
      range("2026-10-13T11:00:00+02:00", "2026-10-13T11:30:00+02:00")
    ),
    appointment(
      "8bbc36b7-a8fc-467c-85eb-cf0bbd97b85a",
      sda,
      range("2026-10-14T16:00:00+02:00", "2026-10-14T17:00:00+02:00")
    ),
    appointment(
      "ba7511ac-50ed-4a2c-89ae-84080fc5ebf0",
      lbo,
      range("2026-10-16T09:00:00+02:00", "2026-10-16T09:30:00+02:00")
    )
  )
