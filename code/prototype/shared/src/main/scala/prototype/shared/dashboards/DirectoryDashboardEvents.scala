package prototype.shared.dashboards

import prototype.shared.appointment.{
  AppointmentCreated,
  AppointmentDeleted,
  AppointmentUpdated
}
import prototype.shared.events.EventType
import prototype.shared.therapist.TherapistCreated

/** The events that can change the directory dashboard: it counts the therapists
  * of the company (all of them, by role, and each with their patients, those
  * without patients too) and its appointments, with the patients seen and
  * active. Left out: a new patient (patients are counted through the
  * appointments, so a patient without one changes nothing), the prestations
  * (billing only), the therapists and patients added to an appointment (sent in
  * the same transaction as the `AppointmentUpdated` that already lists them),
  * and the companies themselves. Not an event: the active patients are those
  * of the last 7 days, so they also change as time passes. The same types as
  * the activity dashboard today, for other reasons, so each has its own set.
  */
object DirectoryDashboardEvents:

  val types: Set[String] = Set(
    EventType.nameOf[TherapistCreated],
    EventType.nameOf[AppointmentCreated],
    EventType.nameOf[AppointmentUpdated],
    EventType.nameOf[AppointmentDeleted]
  )
end DirectoryDashboardEvents
