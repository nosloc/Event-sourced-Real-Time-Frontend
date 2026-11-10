package prototype.client.appointments

import prototype.api.*
import prototype.shared.appointment.*

import org.scalajs.dom

/** A list of appointments (a therapist's, a company's): applies
  * `AppointmentEvolve` to the appointment an event is about, and keeps it only
  * if the key includes it. An appointment already listed keeps its place.
  */
object AppointmentListEvolve:
  def evolve(
      of: AppointmentOf,
      appointments: List[Appointment],
      event: AppointmentEvent
  ): List[Appointment] =
    val current = appointments.find(_.appointmentId.value == event.id)
    val evolveResult = AppointmentEvolve
      .evolve(
        current.map(AppointmentState.fromAppointment),
        event
      )
      .map(
        _.map(AppointmentState.toAppointment)
          .filter(of.includes)
      )

    evolveResult match
      case Left(error) =>
        dom.console.warn(
          s"Appointment event skipped, it does not fit the appointments held: ${error.getMessage}"
        )
        appointments // The event does not fit the current state: ignore it, keep the current state
      case Right(evolved) =>
        updateAppointments(
          appointments,
          event,
          current,
          evolved
        )

  private def updateAppointments(
      appointments: List[Appointment],
      event: AppointmentEvent,
      current: Option[Appointment],
      evolved: Option[Appointment]
  ): List[Appointment] =
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
