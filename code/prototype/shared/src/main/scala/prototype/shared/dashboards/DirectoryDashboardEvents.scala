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
  * active.
  *
  * Left out: a new patient, the prestations, and the companies themselves.
  */
object DirectoryDashboardEvents:

  val types: Set[String] = Set(
    EventType.nameOf[TherapistCreated],
    EventType.nameOf[AppointmentCreated],
    EventType.nameOf[AppointmentUpdated],
    EventType.nameOf[AppointmentDeleted]
  )
end DirectoryDashboardEvents
