package prototype.shared.dashboards

import prototype.shared.appointment.{
  AppointmentDeleted,
  AppointmentUpdated,
  PrestationCreated,
  PrestationDeleted,
  PrestationStatusUpdated
}
import prototype.shared.events.EventType

/** The events that can change the billing dashboard: its amounts come from the
  * prestations, by status and by the week their appointment starts, and its
  * oldest appointments to bill show their start, type and therapists.
  *
  * Left out: a new appointment (it has no prestation yet), a new therapist (no
  * appointment yet), and the patients and companies themselves.
  */
object BillingDashboardEvents:

  val types: Set[String] = Set(
    EventType.nameOf[PrestationCreated],
    EventType.nameOf[PrestationStatusUpdated],
    EventType.nameOf[PrestationDeleted],
    EventType.nameOf[AppointmentUpdated],
    EventType.nameOf[AppointmentDeleted]
  )
end BillingDashboardEvents
