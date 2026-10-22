package prototype.server.core.appointment.api

import cats.effect.IO
import cats.syntax.all.*
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.*
import prototype.server.App
import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.support.{PostgresSuite, TestApp}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import smithy4s.time.LocalDate

/** The appointment service on a real Postgres in Docker: commands, event store,
  * projection and read model together. Every change waits for the projection,
  * so what a call returns, and what is read right after, is already up to
  * date. Docker must be running.
  */
class AppointmentServiceSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val abc = TherapistId("abc")
  private val xyz = TherapistId("xyz")
  private val unknownPatient = PatientId(appointmentId(99))

  // A company, two therapists and two patients, through their services
  private final case class People(patient1: PatientId, patient2: PatientId)

  private def setUp(app: App.Services): IO[People] =
    for
      company <- app.companies
        .createCompany("Example Clinic", Address("Example Street", "1", "Exampleville", "VD"))
        .map(_.companyId)
      _ <- app.therapists.createTherapist("Ann", "Lee", abc, company, Role.PHYSICIAN)
      _ <- app.therapists.createTherapist("Bob", "Ray", xyz, company, Role.PSYCHOLOGIST)
      patient1 <- app.patients
        .createPatient("Camille", "Rochat", LocalDate(1987, 4, 12))
        .map(_.patientId)
      patient2 <- app.patients
        .createPatient("Noah", "Favre", LocalDate(1994, 9, 3))
        .map(_.patientId)
    yield People(patient1, patient2)

  private def withApp[A](test: (App.Services, People) => IO[A]): IO[A] =
    freshDatabase.use(postgres =>
      TestApp.make(postgres).use(app => setUp(app).flatMap(test(app, _)))
    )

  private def create(
      app: App.Services,
      therapists: List[TherapistId],
      patients: List[PatientId],
      timeRange: TimeRange
  ) =
    app.appointments
      .createAppointment(therapists, patients, AppointmentType.INDIVIDUAL, timeRange)
      .map(_.appointment)

  "The appointment service" should "answer a creation with the saved appointment, readable right away" in {
    withApp { (app, people) =>
      for
        created <- create(app, List(abc), List(people.patient1), range(10, 11))
        read <- app.appointments.getAppointment(created.appointmentId).map(_.appointment)
      yield (created, read)
    }.asserting { case (created, read) =>
      created.therapistIds shouldBe List(abc)
      created.timeRange shouldBe range(10, 11)
      created.prestations shouldBe Nil
      read shouldBe created
    }
  }

  it should "turn the refusals into the API's errors" in {
    withApp { (app, people) =>
      for
        _ <- create(app, List(abc), List(people.patient1), range(10, 12))
        results <- List(
          create(app, List(xyz, abc), Nil, range(11, 13)),
          create(app, List(xyz), List(people.patient2, people.patient1), range(11, 13)),
          create(app, List(TherapistId("zzz")), Nil, range(14, 15)),
          create(app, List(xyz), List(unknownPatient), range(14, 15)),
          create(app, List(xyz), Nil, range(15, 14)),
          create(app, Nil, Nil, range(14, 15))
        ).map(_.attempt).sequence
      yield (results, people)
    }.asserting { case (results, people) =>
      results shouldBe List(
        Left(TherapistBusy(List(abc))),
        Left(PatientBusy(List(people.patient1))),
        Left(TherapistsNotFound(List(TherapistId("zzz")))),
        Left(PatientsNotFound(List(unknownPatient))),
        Left(InvalidTimeRange()),
        Left(InvalidParticipants())
      )
    }
  }

  it should "answer an update with the new values, and free the old time" in {
    withApp { (app, people) =>
      for
        created <- create(app, List(abc), List(people.patient1), range(10, 11))
        updated <- app.appointments
          .updateAppointment(
            created.appointmentId,
            AppointmentType.COUPLE,
            List(xyz),
            List(people.patient2),
            range(14, 15)
          )
          .map(_.appointment)
        oldTime <- create(app, List(abc), List(people.patient1), range(10, 11)).attempt
      yield (updated, oldTime.isRight)
    }.asserting { case (updated, oldTimeFree) =>
      (updated.appointmentType, updated.therapistIds, updated.timeRange) shouldBe
        (AppointmentType.COUPLE, List(xyz), range(14, 15))
      oldTimeFree shouldBe true
    }
  }

  it should "run the prestations of an appointment through their statuses" in {
    withApp { (app, _) =>
      for
        created <- create(app, List(abc), Nil, range(10, 11))
        id = created.appointmentId
        added <- app.appointments.createPrestation(id, "Consultation", 1, 120f).map(_.appointment)
        prestation = added.prestations.head.prestationId
        billed <- app.appointments
          .updatePrestationStatus(id, prestation, PrestationStatus.BILLED)
          .map(_.appointment)
        back <- app.appointments
          .updatePrestationStatus(id, prestation, PrestationStatus.TO_BILL)
          .attempt
        invalid <- app.appointments.createPrestation(id, "Nothing", 0, 120f).attempt
        unknown <- app.appointments
          .updatePrestationStatus(id, PrestationId(appointmentId(98)), PrestationStatus.BILLED)
          .attempt
        _ <- app.appointments.deletePrestation(id, prestation)
        after <- app.appointments.getAppointment(id).map(_.appointment)
      yield (added, billed, back, invalid, unknown, after)
    }.asserting { case (added, billed, back, invalid, unknown, after) =>
      added.prestations.map(p => (p.label, p.status)) shouldBe
        List(("Consultation", PrestationStatus.TO_BILL))
      billed.prestations.map(_.status) shouldBe List(PrestationStatus.BILLED)
      back shouldBe Left(InvalidStatusChange())
      invalid shouldBe Left(InvalidPrestation())
      unknown shouldBe Left(PrestationNotFound())
      after.prestations shouldBe Nil
    }
  }

  it should "list a therapist's appointments in time order, and know unknown therapists" in {
    withApp { (app, _) =>
      for
        late <- create(app, List(abc), Nil, range(15, 16))
        early <- create(app, List(abc), Nil, range(9, 10))
        _ <- create(app, List(xyz), Nil, range(12, 13))
        // The therapist itself is read from its own projection
        _ <- TestApp.eventually(app.therapists.getTherapist(abc).attempt)(_.isRight)
        listed <- app.appointments.listTherapistAppointments(abc).map(_.appointments)
        unknown <- app.appointments.listTherapistAppointments(TherapistId("zzz")).attempt
      yield (listed.map(_.appointmentId), List(early, late).map(_.appointmentId), unknown)
    }.asserting { case (listed, expected, unknown) =>
      listed shouldBe expected
      unknown shouldBe Left(TherapistNotFound(TherapistId("zzz")))
    }
  }

  it should "forget a deleted appointment" in {
    withApp { (app, _) =>
      for
        created <- create(app, List(abc), Nil, range(10, 11))
        _ <- app.appointments.deleteAppointment(created.appointmentId)
        read <- app.appointments.getAppointment(created.appointmentId).attempt
        twice <- app.appointments.deleteAppointment(created.appointmentId).attempt
      yield (read, twice)
    }.asserting(_ shouldBe (Left(AppointmentNotFound()), Left(AppointmentNotFound())))
  }

  it should "still have its appointments and prestations after the application restarts" in {
    freshDatabase.use { postgres =>
      for
        saved <- TestApp.make(postgres).use { app =>
          for
            _ <- setUp(app)
            created <- create(app, List(abc), Nil, range(10, 11))
            withPrestation <- app.appointments
              .createPrestation(created.appointmentId, "Consultation", 1, 120f)
              .map(_.appointment)
          yield withPrestation
        }
        // A second application on the same database: nothing is in memory
        read <- TestApp.make(postgres).use(
          _.appointments.getAppointment(saved.appointmentId).map(_.appointment)
        )
      yield (saved, read)
    }.asserting { case (saved, read) => read shouldBe saved }
  }
