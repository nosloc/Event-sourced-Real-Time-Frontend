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
  * oldest appointments to bill show their start, type and therapists. Left
  * out: a new appointment (it has no prestation yet), a new therapist (no
  * appointment yet), the therapists and patients added to an appointment (sent
  * in the same transaction as the `AppointmentUpdated` that already lists
  * them), and the patients and companies themselves. Not an event: the weekly
  * amounts end with the current week, so they also move on each Monday.
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
