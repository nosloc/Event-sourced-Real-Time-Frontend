package prototype.server.core.appointment.persistence

import java.util.UUID

import cats.effect.{IO, Resource}
import cats.effect.testing.scalatest.AsyncIOSpec

import prototype.api.PrestationStatus
import prototype.server.core.appointment.AppointmentFixtures.*
import prototype.server.support.{PostgresSuite, TestDatabase}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import skunk.Session
import skunk.codec.all.{int8, uuid}
import skunk.syntax.all.*

/** The hand-written repository on a real Postgres in Docker: one appointment and
  * its prestations stored in two tables, read back with one join. This is the
  * SQL no other spec checks. Docker must be running.
  */
class AppointmentRepositorySpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val patient = UUID.fromString("00000000-0000-0000-0000-0000000000c1")

  private def appointment(
      n: Int,
      therapists: List[String],
      from: Int,
      prestations: List[PrestationRow] = Nil
  ) =
    AppointmentState(
      appointmentId(n),
      therapists,
      List(patient),
      "INDIVIDUAL",
      millis(from),
      millis(from + 1),
      prestations
    )

  private def prestation(n: Int, status: PrestationStatus = PrestationStatus.TO_BILL) =
    PrestationRow(prestationId(n), s"Prestation $n", 10f * n, n, status)

  // A repository and the raw connection pool on a fresh database
  private def withRepository[A](
      test: (AppointmentRepository, Resource[IO, Session[IO]]) => IO[A]
  ): IO[A] =
    freshDatabase
      .flatMap(TestDatabase.sessions)
      .use(sessions => test(AppointmentRepository.make(sessions), sessions))

  private def save(repository: AppointmentRepository, states: AppointmentState*) =
    repository.persist(states.map(s => s.appointmentId -> s).toMap, Nil)

  private def prestationRows(sessions: Resource[IO, Session[IO]], id: UUID) =
    sessions.use(
      _.unique(sql"SELECT count(*) FROM prestations WHERE appointment_id = $uuid".query(int8))(id)
    )

  "The appointment repository" should "read back what it saved, prestations in order" in {
    val saved = appointment(
      1,
      List("abc", "xyz"),
      10,
      List(prestation(2, PrestationStatus.PAID), prestation(1), prestation(3, PrestationStatus.CANCELLED))
    )
    withRepository { (repository, _) =>
      save(repository, saved) *> repository.find(saved.appointmentId)
    }.asserting(_ shouldBe Some(saved))
  }

  it should "find nothing for an unknown appointment" in {
    withRepository((repository, _) => repository.find(appointmentId(9)))
      .asserting(_ shouldBe None)
  }

  it should "keep an appointment with no patients and no prestations" in {
    val saved = appointment(1, List("abc"), 10).copy(patientIds = Nil)
    withRepository { (repository, _) =>
      save(repository, saved) *> repository.find(saved.appointmentId)
    }.asserting(_ shouldBe Some(saved))
  }

  it should "replace the prestations when the appointment is saved again" in {
    val first = appointment(1, List("abc"), 10, List(prestation(1), prestation(2), prestation(3)))
    val second = first.copy(
      therapistIds = List("xyz"),
      startTime = millis(14),
      endTime = millis(15),
      prestations = List(prestation(3, PrestationStatus.BILLED), prestation(1))
    )
    withRepository { (repository, sessions) =>
      for
        _ <- save(repository, first)
        _ <- save(repository, second)
        found <- repository.find(first.appointmentId)
        rows <- prestationRows(sessions, first.appointmentId)
      yield (found, rows)
    }.asserting(_ shouldBe (Some(second), 2L))
  }

  it should "remove the prestations with their appointment" in {
    val saved = appointment(1, List("abc"), 10, List(prestation(1), prestation(2)))
    withRepository { (repository, sessions) =>
      for
        _ <- save(repository, saved)
        _ <- repository.persist(Map.empty, List(saved.appointmentId))
        found <- repository.find(saved.appointmentId)
        rows <- prestationRows(sessions, saved.appointmentId)
      yield (found, rows)
    }.asserting(_ shouldBe (None, 0L))
  }

  it should "list every appointment with its prestations" in {
    val first = appointment(1, List("abc"), 10, List(prestation(1)))
    val second = appointment(2, List("xyz"), 12)
    withRepository { (repository, _) =>
      save(repository, first, second) *> repository.all
    }.asserting(_.sortBy(_.startTime) shouldBe List(first, second))
  }

  it should "list a therapist's appointments, earliest first, and only theirs" in {
    val late = appointment(1, List("abc"), 15)
    val early = appointment(2, List("xyz", "abc"), 9)
    val other = appointment(3, List("xyz"), 12)
    withRepository { (repository, _) =>
      for
        _ <- save(repository, late, early, other)
        (abc, _) <- repository.forTherapist("abc")
        (xyz, _) <- repository.forTherapist("xyz")
        (nobody, _) <- repository.forTherapist("qrs")
      yield (abc.map(_.appointmentId), xyz.map(_.appointmentId), nobody)
    }.asserting(
      _ shouldBe (
        List(early.appointmentId, late.appointmentId),
        List(early.appointmentId, other.appointmentId),
        Nil
      )
    )
  }
