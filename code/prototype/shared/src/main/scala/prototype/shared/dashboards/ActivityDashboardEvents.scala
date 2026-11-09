package prototype.shared.dashboards

import prototype.shared.appointment.{
  AppointmentCreated,
  AppointmentDeleted,
  AppointmentUpdated
}
import prototype.shared.events.EventType
import prototype.shared.therapist.TherapistCreated

/** The events that can change the activity dashboard: its figures count the
  * appointments, and its list per therapist shows every therapist of the
  * company.
  *
  * Left out: the prestations (billing only)
  */
object ActivityDashboardEvents:

  val types: Set[String] = Set(
    EventType.nameOf[AppointmentCreated],
    EventType.nameOf[AppointmentUpdated],
    EventType.nameOf[AppointmentDeleted],
    EventType.nameOf[TherapistCreated]
  )
end ActivityDashboardEvents
