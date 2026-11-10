package prototype.client.appointments

import java.util.UUID

import prototype.api.Appointment

/** Whose appointments a list holds, and which appointments belong to it. */
enum AppointmentOf:
  case Therapist(trigram: String)
  case Company(companyId: UUID)

  /** A company keeps every appointment the events bring: an event does not say
    * which company it belongs to, so a view filters it later by the company's
    * therapists.
    */
  def includes(appointment: Appointment): Boolean =
    this match
      case Therapist(trigram) =>
        appointment.therapistIds.exists(_.value == trigram)
      case Company(_) => true
