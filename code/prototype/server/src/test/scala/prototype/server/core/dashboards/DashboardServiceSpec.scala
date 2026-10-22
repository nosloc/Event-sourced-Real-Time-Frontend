package prototype.server.core.dashboards

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.core.appointment.InMemoryAppointmentService
import prototype.server.core.company.InMemoryCompanyService
import prototype.server.core.dashboards.DashboardFixtures.{date, patient, timestamp}
import prototype.server.core.therapist.InMemoryTherapistService

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The dashboards as a client of the service sees them, on in-memory company,
  * therapist and appointment services (no database). What matters here is how
  * the data is gathered (which therapists and appointments belong to the
  * company), not the counting, which has its own specs.
  */
class DashboardServiceSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers:

  private val address = Address("Rue du Lac", "12", "Lausanne", "VD")

  private final class World(
      val companies: CompanyService[IO],
      val therapists: InMemoryTherapistService,
      val appointments: InMemoryAppointmentService,
      val dashboards: DashboardServiceImpl
  )

  // A fresh set of services per test
  private val newWorld: IO[World] =
    for
      companies <- InMemoryCompanyService()
      therapists <- InMemoryTherapistService(companies)
      appointments <- InMemoryAppointmentService(therapists)
      dashboards <- DashboardServiceImpl(companies, therapists, appointments)
    yield World(companies, therapists, appointments, dashboards)

  private def addCompany(w: World, name: String): IO[CompanyId] =
    w.companies.createCompany(name, address).map(_.companyId)

  private def addTherapist(w: World, trigram: String, company: CompanyId): IO[TherapistId] =
    w.therapists
      .createTherapist("First", "Last", TherapistId(trigram), company, Role.PSYCHOLOGIST)
      .map(_.therapist.therapistId)

  private def addAppointment(
      w: World,
      therapists: List[TherapistId],
      patients: List[PatientId],
      day: String,
      hour: Int
  ): IO[Appointment] =
    w.appointments
      .createAppointment(
        therapists,
        patients,
        AppointmentType.INDIVIDUAL,
        TimeRange(timestamp(day, hour), timestamp(day, hour + 1))
      )
      .map(_.appointment)

  "Every dashboard" should "fail with CompanyNotFound for an unknown company" in {
    val unknown = CompanyId(java.util.UUID.fromString("00000000-0000-4000-8000-000000000000"))

    for
      w <- newWorld
      directory <- w.dashboards.getDirectoryDashboard(unknown).attempt
      activity <- w.dashboards
        .getActivityDashboard(unknown, PeriodUnit.WEEK, date("2026-10-12"))
        .attempt
      billing <- w.dashboards.getBillingDashboard(unknown).attempt
    yield
      directory shouldBe Left(CompanyNotFound())
      activity shouldBe Left(CompanyNotFound())
      billing shouldBe Left(CompanyNotFound())
  }

  "The directory" should "count only the therapists and appointments of the company" in {
    for
      w <- newWorld
      mine <- addCompany(w, "Mine")
      other <- addCompany(w, "Other")
      arb <- addTherapist(w, "arb", mine)
      mdp <- addTherapist(w, "mdp", mine)
      out <- addTherapist(w, "out", other)
      ann = patient(1)
      bob = patient(2)
      _ <- addAppointment(w, List(arb), List(ann), "2026-10-12", 9)
      _ <- addAppointment(w, List(mdp), List(bob), "2026-10-12", 9)
      _ <- addAppointment(w, List(out), List(bob), "2026-10-13", 9)
      result <- w.dashboards.getDirectoryDashboard(mine)
    yield
      result.therapists shouldBe 2
      result.appointments shouldBe 2
      result.patientsSeen shouldBe 2
  }

  it should "count an appointment once when two therapists of the company are in it" in {
    for
      w <- newWorld
      mine <- addCompany(w, "Mine")
      arb <- addTherapist(w, "arb", mine)
      mdp <- addTherapist(w, "mdp", mine)
      ann = patient(1)
      _ <- addAppointment(w, List(arb, mdp), List(ann), "2026-10-12", 9)
      result <- w.dashboards.getDirectoryDashboard(mine)
    yield
      result.appointments shouldBe 1
      result.patientsPerTherapist.map(_.count) shouldBe List(1, 1)
  }

  it should "count an appointment for each company that has one of its therapists" in {
    for
      w <- newWorld
      a <- addCompany(w, "A")
      b <- addCompany(w, "B")
      arb <- addTherapist(w, "arb", a)
      mdp <- addTherapist(w, "mdp", b)
      ann = patient(1)
      _ <- addAppointment(w, List(arb, mdp), List(ann), "2026-10-12", 9)
      forA <- w.dashboards.getDirectoryDashboard(a)
      forB <- w.dashboards.getDirectoryDashboard(b)
    yield
      forA.appointments shouldBe 1
      forB.appointments shouldBe 1
  }

  "The activity dashboard" should "count the company's appointments of the chosen week" in {
    for
      w <- newWorld
      mine <- addCompany(w, "Mine")
      arb <- addTherapist(w, "arb", mine)
      ann = patient(1)
      _ <- addAppointment(w, List(arb), List(ann), "2026-10-13", 9)
      _ <- addAppointment(w, List(arb), List(ann), "2026-10-14", 9)
      _ <- addAppointment(w, List(arb), List(ann), "2026-10-21", 9)
      result <- w.dashboards.getActivityDashboard(mine, PeriodUnit.WEEK, date("2026-10-14"))
    yield
      result.appointments shouldBe 2
      result.patientsSeen shouldBe 1
      result.busiestTherapist shouldBe Some(TherapistCount(arb, 2))
  }

  "The billing dashboard" should "sum the prestations of the company by status" in {
    for
      w <- newWorld
      mine <- addCompany(w, "Mine")
      other <- addCompany(w, "Other")
      arb <- addTherapist(w, "arb", mine)
      mdp <- addTherapist(w, "mdp", mine)
      out <- addTherapist(w, "out", other)
      ann = patient(1)
      shared <- addAppointment(w, List(arb, mdp), List(ann), "2026-10-12", 9)
      foreign <- addAppointment(w, List(out), List(ann), "2026-10-12", 11)
      _ <- w.appointments.createPrestation(shared.appointmentId, "Consultation", 2, 50f)
      withPrestation <- w.appointments.createPrestation(shared.appointmentId, "Report", 1, 30f)
      _ <- w.appointments.createPrestation(foreign.appointmentId, "Consultation", 1, 999f)
      reportId = withPrestation.appointment.prestations.find(_.label == "Report").get.prestationId
      _ <- w.appointments.updatePrestationStatus(shared.appointmentId, reportId, PrestationStatus.BILLED)
      result <- w.dashboards.getBillingDashboard(mine)
    yield
      // The appointment with two therapists of the company is counted once,
      // and the other company's prestation is left out
      result.amountPerStatus shouldBe
        AmountPerStatus(toBill = 100f, billed = 30f, paid = 0f, cancelled = 0f)
      result.oldestToBill.map(_.appointmentId) shouldBe List(shared.appointmentId)
  }
