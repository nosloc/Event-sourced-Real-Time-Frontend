package prototype.server.core.appointment

import java.util.UUID

import prototype.api.TimeRange

import smithy4s.time.Timestamp

/** Fixed ids and times for the appointment specs. Times are counted from the
  * epoch, so "10:30" is 10 hours 30 minutes after it.
  */
object AppointmentFixtures:

  def at(hour: Int, minute: Int = 0): Timestamp =
    Timestamp.fromEpochSecond(hour * 3600L + minute * 60L)

  // The same instant in epoch milliseconds, as the events store it
  def millis(hour: Int, minute: Int = 0): Long =
    (hour * 3600L + minute * 60L) * 1000L

  def range(fromHour: Int, toHour: Int): TimeRange =
    TimeRange(at(fromHour), at(toHour))

  def appointmentId(n: Int): UUID =
    UUID.fromString(f"00000000-0000-0000-0000-0000000001$n%02d")

  def prestationId(n: Int): UUID =
    UUID.fromString(f"00000000-0000-0000-0000-0000000002$n%02d")
