package prototype.client.views.eventsourcing

import prototype.api.{EventView, EventsPage}
import smithy4s.Blob
import smithy4s.json.Json

/** The rules of the event sourcing tab, without state: the pages, the texts
  * of the pager, and how live events join the ones already held.
  */
object EventSourcingModel:
  enum EventSourcingPage:
    case LivePage
    // Counted from the newest event, from 2: page 1 is the live window
    case HistoryPage(number: Int)

  import EventSourcingPage.*

  /** The most events one page can hold on the server */
  val MaxEventsSize = 200

  val PageSizes: List[Int] = List(10, 25, 50, 100, MaxEventsSize)

  /** Events from skip `from` (included) to `to` (excluded), oldest first */
  final case class ShownHistoryPage(
      from: Long,
      to: Long,
      events: List[EventView]
  ):
    def covers(skip: Long, size: Long): Boolean =
      from <= skip && skip + size <= to
    def slice(skip: Long, size: Long): List[EventView] =
      events.drop((skip - from).toInt).take(size.toInt)

  /** Where a move leads, and the number of events to freeze the pages at when
    * it leaves the live window
    */
  final case class Move(page: EventSourcingPage, anchor: Option[Long] = None)

  /** What the pager shows depends on: the page, the number of events, the
    * page size, and the number of events when the live window was left
    */
  final case class PagerState(
      page: EventSourcingPage,
      total: Long,
      size: Int,
      anchor: Long
  )

  // Pages

  /** Page `number` counted from the newest of `anchor` events: its skip from
    * the oldest event and how many events it holds
    */
  def pageBounds(number: Int, size: Int, anchor: Long): (Long, Int) =
    val end = (anchor - (number - 1).toLong * size).max(0)
    val start = (end - size).max(0)
    (start, (end - start).toInt)

  def pageCount(anchor: Long, size: Int): Int =
    ((anchor + size - 1) / size).max(1).toInt

  // Page 1 would be the live window frozen: the live window is shown instead
  def older(page: EventSourcingPage, total: Long): Move =
    page match
      case LivePage            => Move(HistoryPage(2), Some(total))
      case HistoryPage(number) => Move(HistoryPage(number + 1))

  def newer(page: EventSourcingPage): EventSourcingPage =
    page match
      case HistoryPage(number) if number > 2 => HistoryPage(number - 1)
      case _                                 => LivePage

  def oldest(
      page: EventSourcingPage,
      size: Int,
      total: Long,
      anchor: Long
  ): Move =
    page match
      case LivePage =>
        Move(HistoryPage(pageCount(total, size)), Some(total))
      case HistoryPage(_) => Move(HistoryPage(pageCount(anchor, size)))

  /** On a page, the one that holds its top row at the new size */
  def resized(
      page: EventSourcingPage,
      oldSize: Int,
      newSize: Int
  ): EventSourcingPage =
    page match
      case LivePage => LivePage
      case HistoryPage(number) =>
        val topRank = (number - 1).toLong * oldSize
        val newNumber = (topRank / newSize).toInt + 1
        if newNumber == 1 then LivePage else HistoryPage(newNumber)

  def canOlder(
      page: EventSourcingPage,
      total: Long,
      size: Int,
      anchor: Long
  ): Boolean =
    page match
      case LivePage            => total > size
      case HistoryPage(number) => number < pageCount(anchor, size)

  // Pager texts

  def pageLabel(
      page: EventSourcingPage,
      size: Int,
      anchor: Long
  ): Option[String] =
    page match
      case LivePage => None
      case HistoryPage(number) =>
        Some(s"Page $number of ${pageCount(anchor, size)}")

  /** Ranks from the newest event, the order of the table: "51–100 of 12932" */
  def rangeText(
      page: EventSourcingPage,
      total: Long,
      size: Int,
      anchor: Long
  ): String =
    page match
      case LivePage if total == 0 => "0 of 0"
      case LivePage               => s"1–${size.toLong.min(total)} of $total"
      case HistoryPage(number) =>
        val first = (number - 1).toLong * size + 1
        s"$first–${(number.toLong * size).min(anchor)} of $anchor"

  /** On a page: the events written since the live window was left */
  def newEvents(
      page: EventSourcingPage,
      total: Long,
      anchor: Long
  ): Option[Long] =
    page match
      case HistoryPage(_) if total > anchor => Some(total - anchor)
      case _                                => None

  // Live events

  /** A message of the stream: one event as JSON */
  def decode(raw: String): Option[EventView] =
    Json.read[EventView](Blob(raw)).toOption

  /** Not held yet, and not older than the whole held window (a replay) */
  def isNewEvent(held: List[EventView], event: EventView): Boolean =
    !held.exists(_.eventId == event.eventId) &&
      held.lastOption.forall(_.position < event.position)

  /** The live events already held and newer than the page are not in its
    * total
    */
  def totalAfterLatest(page: EventsPage, held: List[EventView]): Long =
    val pageNewest = page.events.map(_.position).maxOption.getOrElse(0L)
    page.total + held.count(_.position > pageNewest)

  /** Both lists without duplicates, newest first, at most `MaxEventsSize` */
  def mergeEvents(
      oldEvents: List[EventView],
      newEvents: List[EventView]
  ): List[EventView] =
    val allEvents: Map[String, EventView] =
      (oldEvents ++ newEvents).map(e => e.eventId -> e).toMap
    allEvents.values.toList
      .sortBy(-_.position)
      .take(MaxEventsSize)
  end mergeEvents

  def mergeEvents(
      oldEvents: List[EventView],
      newEvent: EventView
  ): List[EventView] =
    mergeEvents(oldEvents, List(newEvent))
  end mergeEvents
end EventSourcingModel
