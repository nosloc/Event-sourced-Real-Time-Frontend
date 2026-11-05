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
  * company, those without appointments too. Left out: the prestations (billing
  * only), the patients and companies themselves, and the therapists and
  * patients added to an appointment, sent in the same transaction as the
  * `AppointmentUpdated` that already lists them.
  */
object ActivityDashboardEvents:

  val types: Set[String] = Set(
    EventType.nameOf[AppointmentCreated],
    EventType.nameOf[AppointmentUpdated],
    EventType.nameOf[AppointmentDeleted],
    EventType.nameOf[TherapistCreated]
  )
end ActivityDashboardEvents
