package prototype.server.sync

import java.time.LocalDate
import java.util.UUID

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

import prototype.api.Cursor
import prototype.server.core.PrototypeEventCodec
import prototype.server.support.InMemoryEventStore
import prototype.shared.company.CompanyCreated
import prototype.shared.events.PrototypeEvent
import prototype.shared.patient.PatientCreated

import persistent4s.{EventTypeName, PendingEvent, ReadCursor}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The newest load on the in-memory event store: every event is in commit group
  * 0, and the n-th appended is at position n. The snapshot is a fixed value
  * with the checkpoint each case needs.
  */
class NewestLoadSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  private val codec = PrototypeEventCodec.eventCodec

  private val companies =
    ReadModel("companies", Set(EventTypeName.of[CompanyCreated]))

  private def company(name: String): CompanyCreated =
    CompanyCreated(UUID.randomUUID(), name, "Example Street", "1", "Exampleville", "VD")

  private val patient: PatientCreated =
    PatientCreated(UUID.randomUUID(), "Ada", "Example", LocalDate.of(1990, 1, 1))

  // Appends the events one by one, so the n-th is at position n
  private def storeWith(
      events: PrototypeEvent*
  ): IO[InMemoryEventStore[PrototypeEvent]] =
    InMemoryEventStore.make[PrototypeEvent].flatTap(store =>
      events.toList.traverse_(event =>
        store.appendUnchecked(
          List(
            PendingEvent(event, Set.empty, EventTypeName.fromInstance(event), isExternal = false)
          )
        )
      )
    )

  private def load(
      store: InMemoryEventStore[PrototypeEvent],
      checkpoint: Long,
      pageSize: Int = NewestLoad.PageSize,
      maxEvents: Int = NewestLoad.MaxEvents
  ): IO[Loaded[String]] =
    NewestLoad(store, codec, pageSize, maxEvents)(companies)(
      IO.pure(("snapshot", ReadCursor(0L, checkpoint)))
    )

  private def positions(loaded: Loaded[String]): List[Long] =
    loaded.catchUp.events.map(_.cursor.position)

  "The newest load" should "send no event, and the checkpoint as head, when the read model is up to date" in {
    for
      store <- storeWith(company("A"), company("B"))
      loaded <- load(store, checkpoint = 2L)
    yield
      loaded.snapshot shouldBe "snapshot"
      loaded.cursor shouldBe Cursor(0L, 2L)
      loaded.catchUp.events shouldBe empty
      loaded.catchUp.head shouldBe Cursor(0L, 2L)
  }

  it should "send the read model's events after the checkpoint, in order, the last one as head" in {
    for
      store <- storeWith(company("A"), company("B"), patient, company("C"))
      loaded <- load(store, checkpoint = 1L)
    yield
      loaded.cursor shouldBe Cursor(0L, 1L)
      positions(loaded) shouldBe List(2L, 4L)
      loaded.catchUp.events.map(_.eventType).distinct shouldBe List("CompanyCreated")
      loaded.catchUp.head shouldBe Cursor(0L, 4L)
  }

  it should "keep the checkpoint as head when only other read models' events came after it" in {
    for
      store <- storeWith(company("A"), patient)
      loaded <- load(store, checkpoint = 1L)
    yield
      loaded.catchUp.events shouldBe empty
      loaded.catchUp.head shouldBe Cursor(0L, 1L)
  }

  it should "read a catch-up longer than one page whole" in {
    for
      five <- storeWith((1 to 5).map(i => company(s"C$i"))*)
      fromFive <- load(five, checkpoint = 0L, pageSize = 2)
      // Fills its last page exactly: an empty page then ends the read
      four <- storeWith((1 to 4).map(i => company(s"C$i"))*)
      fromFour <- load(four, checkpoint = 0L, pageSize = 2)
    yield
      positions(fromFive) shouldBe List(1L, 2L, 3L, 4L, 5L)
      fromFive.catchUp.head shouldBe Cursor(0L, 5L)
      positions(fromFour) shouldBe List(1L, 2L, 3L, 4L)
      fromFour.catchUp.head shouldBe Cursor(0L, 4L)
  }

  it should "fail, not cut the catch-up short, when it holds more events than the cap" in {
    for
      atCap <- storeWith((1 to 3).map(i => company(s"C$i"))*)
      fromAtCap <- load(atCap, checkpoint = 0L, pageSize = 2, maxEvents = 3)
      overCap <- storeWith((1 to 4).map(i => company(s"C$i"))*)
      fromOverCap <- load(overCap, checkpoint = 0L, pageSize = 2, maxEvents = 3).attempt
    yield
      positions(fromAtCap) shouldBe List(1L, 2L, 3L)
      fromOverCap.left.toOption.get shouldBe a[TooFarBehind]
  }
end NewestLoadSpec
