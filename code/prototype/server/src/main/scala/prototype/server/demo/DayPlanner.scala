package prototype.server.demo

import java.time.{Duration, Instant, LocalDate, LocalTime, ZoneId}
import java.time.DayOfWeek.{SATURDAY, SUNDAY}

import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.all.*

/** Plans the appointments of one therapist's day: random, non-overlapping slots
  * on a 15-minute grid between 08:00 and 18:00, in Swiss time.
  */
object DayPlanner:

  val zone: ZoneId = ZoneId.of("Europe/Zurich")

  private val dayStart: LocalTime = LocalTime.of(8, 0)
  private val dayEnd: LocalTime = LocalTime.of(18, 0)
  private val slotMinutes: Int = 15
  private val durationsMinutes: List[Int] = List(15, 30, 45, 60, 90)

  // Every possible start time of the day: 08:00, 08:15, ..., 17:45
  private val slotStarts: List[LocalTime] =
    val slotCount =
      Duration.between(dayStart, dayEnd).toMinutes.toInt / slotMinutes
    List.tabulate(slotCount)(i => dayStart.plusMinutes(i.toLong * slotMinutes))

  /** Every weekday from `from` to `to`, both inclusive. */
  def weekdays(from: LocalDate, to: LocalDate): List[LocalDate] =
    Iterator
      .iterate(from)(_.plusDays(1))
      .takeWhile(!_.isAfter(to))
      .filterNot(day =>
        day.getDayOfWeek == SATURDAY || day.getDayOfWeek == SUNDAY
      )
      .toList

  /** Up to `count` non-overlapping slots on `date` (fewer if the day is full),
    * sorted by start. They start on the 15-minute grid and end by 18:00.
    */
  def day(
      random: Random[IO],
      date: LocalDate,
      count: Int
  ): IO[List[(Instant, Instant)]] =
    for
      starts <- random.shuffleList(slotStarts)
      durations <- starts.traverse(_ => random.elementOf(durationsMinutes))
      candidates = starts
        .zip(durations)
        .map((start, minutes) => (start, start.plusMinutes(minutes.toLong)))
    yield nonOverlapping(candidates, count).map((start, end) =>
      (instant(date, start), instant(date, end))
    )

  /** Keeps candidates in order, skipping any that ends after 18:00 or overlaps
    * an already kept one, until `count` are kept. Result sorted by start time.
    */
  private def nonOverlapping(
      candidates: List[(LocalTime, LocalTime)],
      count: Int
  ): List[(LocalTime, LocalTime)] =
    candidates
      .filterNot((_, end) => end.isAfter(dayEnd))
      .foldLeft(List.empty[(LocalTime, LocalTime)]) {
        case (kept, (start, end)) =>
          val overlaps = kept.exists((keptStart, keptEnd) =>
            start.isBefore(keptEnd) && keptStart.isBefore(end)
          )
          if kept.size >= count || overlaps then kept else (start, end) :: kept
      }
      .sortBy((start, _) => start.toSecondOfDay)

  private def instant(date: LocalDate, time: LocalTime): Instant =
    date.atTime(time).atZone(zone).toInstant
