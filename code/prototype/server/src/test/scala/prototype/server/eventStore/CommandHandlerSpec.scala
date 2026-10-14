package prototype.server.eventStore

import java.util.UUID
import cats.effect.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.testing.scalatest.AsyncIOSpec
import prototype.api.*
import prototype.server.core.appointment.*
import prototype.server.core.appointment.appointmentModel.*
import smithy4s.time.Timestamp

/** The command handler is the contract between the decider, the store and the
  * view. These tests use the real appointment decider, viewer and the in-memory
  * store (no mocks), and check what the handler promises:
  *   - a valid command is stored and shown in the view;
  *   - a rejected command changes nothing;
  *   - the state used to decide comes from the stored events;
  *   - replaying the stored events gives the same state as the view.
  */
class CommandHandlerSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val therapists = List(TherapistId("tst"))
  private val patients =
    List(PatientId(UUID.fromString("0b2f6d5e-5b0a-4a43-9a9e-3a6b0a1c2d3e")))
  private val kind = AppointmentType.INDIVIDUAL

  private def range(from: Int, to: Int): TimeRange =
    TimeRange(
      Timestamp.fromEpochSecond(from * 3600L),
      Timestamp.fromEpochSecond(to * 3600L)
    )

  private def newId(): AppointmentId = AppointmentId(UUID.randomUUID())
  private def tag(id: AppointmentId): String = id.value.toString

  private val morning = range(9, 10)
  private val afternoon = range(14, 15)

  // A fresh store, view and handler for every test, so tests cannot affect
  // each other.
  private class Setup(
      val store: InMemoryEventStore,
      val viewer: AppointmentViewer,
      val handler: CommandHandler[
        AppointmentState,
        AppointmentCommand,
        AppointmentEvent,
        AppointmentError
      ]
  )

  private val setup: IO[Setup] =
    for
      store <- InMemoryEventStore.create
      view <- Ref.of[IO, Map[String, Appointment]](Map.empty)
      viewer = AppointmentViewer(view)
    yield new Setup(store, viewer, new CommandHandler(AppointmentDecider(), viewer, store))

  "handle" should "store the events and update the view for a valid command" in {
    val id = newId()
    val test =
      for
        s <- setup
        result <- s.handler.handle(tag(id), CreateAppointmentCommand(id, therapists, patients, kind, morning))
        stored <- s.store.load(tag(id))
        shown <- s.viewer.getState(tag(id))
      yield (result, stored, shown)

    test.asserting { case (result, stored, shown) =>
      val expected = Appointment(id, therapists, patients, kind, morning)
      result shouldBe Right(Some(expected))
      stored shouldBe List(AppointmentCreated(id, therapists, patients, kind, morning))
      shown shouldBe Some(expected)
    }
  }

  it should "leave the store and the view unchanged for a rejected command" in {
    val id = newId()
    val test =
      for
        s <- setup
        result <- s.handler.handle(tag(id), DeleteAppointmentCommand(id))
        stored <- s.store.load(tag(id))
        shown <- s.viewer.getState(tag(id))
      yield (result, stored, shown)

    test.asserting { case (result, stored, shown) =>
      result shouldBe Left(NOT_FOUND)
      stored shouldBe Nil
      shown shouldBe None
    }
  }

  it should "decide from the stored events, so a second create is rejected" in {
    val id = newId()
    val test =
      for
        s <- setup
        _ <- s.handler.handle(tag(id), CreateAppointmentCommand(id, therapists, patients, kind, morning))
        second <- s.handler.handle(tag(id), CreateAppointmentCommand(id, therapists, patients, kind, afternoon))
        stored <- s.store.load(tag(id))
      yield (second, stored)

    test.asserting { case (second, stored) =>
      second shouldBe Left(ALREADY_EXISTS)
      // The rejected command added nothing: only the first event is stored
      stored shouldBe List(AppointmentCreated(id, therapists, patients, kind, morning))
    }
  }

  it should "keep the full history in the store after a delete" in {
    val id = newId()
    val test =
      for
        s <- setup
        _ <- s.handler.handle(tag(id), CreateAppointmentCommand(id, therapists, patients, kind, morning))
        _ <- s.handler.handle(tag(id), UpdateAppointmentCommand(id, kind, afternoon))
        deleted <- s.handler.handle(tag(id), DeleteAppointmentCommand(id))
        stored <- s.store.load(tag(id))
        shown <- s.viewer.getState(tag(id))
      yield (deleted, stored, shown)

    test.asserting { case (deleted, stored, shown) =>
      deleted shouldBe Right(None)
      // Events are never removed: the delete is one more event
      stored shouldBe List(
        AppointmentCreated(id, therapists, patients, kind, morning),
        AppointmentUpdated(id, kind, afternoon),
        AppointmentDeleted(id)
      )
      shown shouldBe None
    }
  }

  it should "keep the streams of different appointments apart" in {
    val first = newId()
    val second = newId()
    val test =
      for
        s <- setup
        _ <- s.handler.handle(tag(first), CreateAppointmentCommand(first, therapists, patients, kind, morning))
        _ <- s.handler.handle(tag(second), CreateAppointmentCommand(second, therapists, patients, kind, afternoon))
        storedFirst <- s.store.load(tag(first))
        storedSecond <- s.store.load(tag(second))
      yield (storedFirst, storedSecond)

    test.asserting { case (storedFirst, storedSecond) =>
      storedFirst shouldBe List(AppointmentCreated(first, therapists, patients, kind, morning))
      storedSecond shouldBe List(AppointmentCreated(second, therapists, patients, kind, afternoon))
    }
  }

  it should "give a view equal to replaying the stored events" in {
    // The property event sourcing relies on: the state held by the view is
    // exactly what folding the stored events produces.
    val id = newId()
    val test =
      for
        s <- setup
        _ <- s.handler.handle(tag(id), CreateAppointmentCommand(id, therapists, patients, kind, morning))
        _ <- s.handler.handle(tag(id), UpdateAppointmentCommand(id, kind, afternoon))
        stored <- s.store.load(tag(id))
        shown <- s.viewer.getState(tag(id))
      yield (stored, shown)

    test.asserting { case (stored, shown) =>
      val replayed = stored
        .collect { case e: AppointmentEvent => e }
        .foldLeft(Option.empty[Appointment])(appointmentModel.evolve)

      replayed shouldBe shown
      replayed shouldBe Some(Appointment(id, therapists, patients, kind, afternoon))
    }
  }
