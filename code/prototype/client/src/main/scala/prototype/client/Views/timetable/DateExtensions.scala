package prototype.client.Views.Timetable

import scala.scalajs.js.Date

extension (d: Date)
  def dateToString: String =
    val y = d.getFullYear().toInt
    val m = d.getMonth().toInt + 1
    val day = d.getDate().toInt
    f"$y%04d-$m%02d-$day%02d"
  def dateToStringShort: String =
    val m = d.getMonth().toInt + 1
    val day = d.getDate().toInt
    f"$m%02d-$day%02d"
  def getTimeString: String =
    val hours = d.getHours().toInt
    val minutes = d.getMinutes().toInt
    f"$hours%02d:$minutes%02d"
  def sameDayAs(other: Date): Boolean =
    d.getFullYear() == other.getFullYear() &&
      d.getMonth() == other.getMonth() &&
      d.getDate() == other.getDate()
  def decimalHours: Double =
    d.getHours().toDouble + (d.getMinutes().toDouble / 60.0)
  def getWeekDayIndex: Int =
    (d.getDay().toInt + 6) % 7 // 0 = Monday, 6 = Sunday

given Ordering[Date] = Ordering.by(_.getTime())
