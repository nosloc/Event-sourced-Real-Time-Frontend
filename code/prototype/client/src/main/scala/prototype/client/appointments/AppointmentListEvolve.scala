package prototype.client.appointments

import prototype.api.*

/** A therapist's appointments: applies `AppointmentEvolve` to the appointment
  * an event is about, and keeps it only if the therapist is in it. An
  * appointment already listed keeps its place.
  */
object AppointmentListEvolve:
  def evolve(
      trigram: String,
      appointments: List[Appointment],
      event: AppointmentEvent
  ): List[Appointment] =
    val current = appointments.find(_.appointmentId.value == event.id)
    val evolved = AppointmentEvolve
      .evolve(
        current,
        event
      )
      .filter(_.therapistIds.contains(trigram))

    evolved match
      case Some(updated) if current.isDefined =>
        appointments.map(a =>
          if a.appointmentId.value == event.id then updated
          else a
        )
      case Some(created) =>
        created :: appointments
      case None if current.isDefined =>
        appointments.filterNot(_.appointmentId.value == event.id)
      case None =>
        appointments
