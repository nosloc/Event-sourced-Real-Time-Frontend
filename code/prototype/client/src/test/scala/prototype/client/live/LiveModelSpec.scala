package prototype.client.live

import prototype.api.Cursor
import prototype.client.live.LiveModel.{Input, MaxTailSize, State}
import prototype.shared.Cursors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `LiveModel.step` on a tiny model: one key ("k"), the value is the list of
  * the events applied, and each event is its own position (in commit group 0,
  * so delivery order is position order), so the value shows which events were
  * applied and in which order.
  */
class LiveModelSpec extends AnyFlatSpec with Matchers:

  private type Model = State[String, List[Long], Long]
  private type In = Input[String, List[Long], Long]

  private val evolve: (String, List[Long], Long) => List[Long] =
    (_, applied, event) => applied :+ event

  private val initial: Model = LiveModel.initial[String, List[Long], Long]

  // The state after these inputs, from `start`
  private def run(start: Model, inputs: Seq[In]): Model =
    inputs.foldLeft(start)(LiveModel.step(evolve))

  // The cursor of the event at this position
  private def cursorAt(position: Long): Cursor = Cursor(0L, position)

  private def received(positions: Seq[Long]): Seq[In] =
    positions.map(p => Input.Received(cursorAt(p), p))

  private def snapshot(upTo: Long): In =
    Input.Loaded("k", (1L to upTo).toList, cursorAt(upTo))

  // A load of "k" during which `burst` events arrive (positions 1 to `burst`),
  // answered by a snapshot at position `at`
  private def loadDuring(burst: Long, at: Long): Model =
    run(
      initial,
      Seq(Input.Started("k")) ++ received(1L to burst) :+ snapshot(at)
    )

  "A load" should "apply the events received during it, after the snapshot" in {
    val state = loadDuring(burst = 3L, at = 2L)
    state.value shouldBe Some(List(1L, 2L, 3L))
    state.cursor shouldBe cursorAt(3L)
    state.loading shouldBe false
  }

  it should "then apply the events received after it" in {
    val state = run(loadDuring(burst = 3L, at = 2L), received(Seq(4L)))
    state.value shouldBe Some(List(1L, 2L, 3L, 4L))
    state.cursor shouldBe cursorAt(4L)
  }

  it should "be accepted from an empty projection (position -1)" in {
    // Nothing was dropped: "nothing dropped" must not compare above -1
    val state =
      run(initial, Seq(Input.Started("k"), Input.Loaded("k", Nil, cursorAt(-1L))))
    state.value shouldBe Some(Nil)
    state.refetches shouldBe 0
    state.loading shouldBe false
  }

  "A load whose later events were dropped from the tail" should
    "be rejected, show the snapshot alone, and ask for a new load" in {
      // MaxTailSize + 50 events arrive: 1 to 50 are dropped, 50 > 10
      val state = loadDuring(burst = MaxTailSize + 50L, at = 10L)
      state.refetches shouldBe 1
      state.loading shouldBe true
      // The snapshot is newer than the empty screen: shown, without the tail
      state.value shouldBe Some((1L to 10L).toList)
      state.cursor shouldBe cursorAt(10L)
    }

  it should "keep the value frozen until a load is accepted" in {
    val rejected = loadDuring(burst = MaxTailSize + 50L, at = 10L)
    val state = run(rejected, received(Seq(MaxTailSize + 51L)))
    state.value shouldBe rejected.value
    state.cursor shouldBe rejected.cursor
    // The event is kept in the tail for the next snapshot
    state.tail.last shouldBe (cursorAt(MaxTailSize + 51L) -> (MaxTailSize + 51L))
  }

  it should "be completed by a newer snapshot, and the counter reset" in {
    val last = MaxTailSize + 51L
    val rejected = loadDuring(burst = MaxTailSize + 50L, at = 10L)
    val state = run(
      rejected,
      received(Seq(last)) ++ Seq(Input.Started("k"), snapshot(200L))
    )
    // 51 to `last` are still in the tail: 201 to `last` are applied
    state.value shouldBe Some((1L to last).toList)
    state.cursor shouldBe cursorAt(last)
    state.refetches shouldBe 0
    state.loading shouldBe false
  }

  it should "keep a newer value on screen rather than an older snapshot" in {
    // Loaded and live up to 400; the tail rolled, dropping events up to 144
    val live = run(loadDuring(burst = 300L, at = 300L), received(301L to 400L))
    live.cursor shouldBe cursorAt(400L)
    // A reload (a reconnect) answered at 100, before the events dropped
    val state = run(live, Seq(Input.Started("k"), snapshot(100L)))
    state.refetches shouldBe 1
    state.value shouldBe live.value
    state.cursor shouldBe cursorAt(400L)
  }

  // Loaded and live up to 100, then a reconnect: 101 to 110 were published
  // while disconnected and never sent, 111 arrives during the reload
  private def reconnectedAt100: Model =
    run(
      initial,
      Seq(Input.Started("k"), snapshot(100L), Input.Started("k")) ++
        received(Seq(111L))
    )

  // The events missing before the last one applied: empty when the value is
  // the state at some position
  private def holes(state: Model): List[Long] =
    state.value.toList.flatMap(applied =>
      (1L to applied.lastOption.getOrElse(0L)).toList.diff(applied)
    )

  "A reload on the same key (a reconnect)" should
    "never apply an event over missing ones" in {
      val state = reconnectedAt100
      state.loading shouldBe true
      holes(state) shouldBe Nil
    }

  it should "hold each event once after the snapshot" in {
    val state = run(reconnectedAt100, Seq(snapshot(110L)))
    state.value shouldBe Some((1L to 111L).toList)
    state.cursor shouldBe cursorAt(111L)
  }

  it should "never apply an event over missing ones after a failed load" in {
    val state = run(
      reconnectedAt100,
      Input.Failed("k", new RuntimeException("load failed")) +:
        received(Seq(112L))
    )
    state.loading shouldBe false
    holes(state) shouldBe Nil
  }

  "A load during which the stream closed" should
    "keep the value frozen, even if an event comes before the next reload" in {
      // Live at 100, a reload read at 100, then the stream closes (101 to 110
      // missed), the reload answers, and 111 arrives before the next reload
      val straddled = run(
        initial,
        Seq(
          Input.Started("k"),
          snapshot(100L),
          Input.Started("k"),
          Input.Closed,
          snapshot(100L)
        ) ++ received(Seq(111L))
      )
      straddled.behind shouldBe true
      holes(straddled) shouldBe Nil
      // The reload after the reopening completes it
      val state = run(straddled, Seq(Input.Started("k"), snapshot(110L)))
      state.value shouldBe Some((1L to 111L).toList)
      state.behind shouldBe false
    }

  "A new key" should "start from nothing" in {
    val rejected = loadDuring(burst = MaxTailSize + 50L, at = 10L)
    val state = run(rejected, Seq(Input.Started("other")))
    state.key shouldBe Some("other")
    state.value shouldBe None
    state.cursor shouldBe Cursors.Start
    state.refetches shouldBe 0
    state.loading shouldBe true
  }
