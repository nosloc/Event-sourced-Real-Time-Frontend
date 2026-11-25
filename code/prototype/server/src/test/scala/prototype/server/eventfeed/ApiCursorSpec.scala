package prototype.server.eventfeed

import prototype.api.Cursor
import prototype.server.eventfeed.ApiCursor.toApi
import prototype.shared.Cursors

import persistent4s.ReadCursor

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** P4S's cursor as the API gives it out, and the API's order (`Cursors`, shared
  * with the browser) against P4S's own: they must agree, or the browser would
  * order events differently from the store.
  */
class ApiCursorSpec extends AnyFlatSpec with Matchers:

  // Includes a late commit: (5, 3) has a lower position than (4, 7) but comes
  // after it in delivery order
  private val cursors = List(
    ReadCursor.Start,
    ReadCursor(0L, 1L),
    ReadCursor(0L, 2L),
    ReadCursor(4L, 7L),
    ReadCursor(5L, 3L),
    ReadCursor(5L, 8L),
    ReadCursor(9L, 4L)
  )

  "ApiCursor.toApi" should "keep the commit group and the position" in {
    ReadCursor(3L, 9L).toApi shouldBe Cursor(commitGroup = 3L, position = 9L)
  }

  "Cursors.ordering" should "order every pair as P4S's ReadCursor ordering does" in {
    for
      a <- cursors
      b <- cursors
    do
      withClue(s"$a vs $b: ") {
        Integer.signum(Cursors.ordering.compare(a.toApi, b.toApi)) shouldBe
          Integer.signum(Ordering[ReadCursor].compare(a, b))
      }
  }

  it should "put a late commit after an event with a higher position" in {
    Cursors.ordering.lt(Cursor(4L, 7L), Cursor(5L, 3L)) shouldBe true
  }
