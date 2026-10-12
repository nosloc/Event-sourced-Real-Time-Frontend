package prototype.server.core.appointment

import cats.effect.*
import prototype.server.eventStore.Viewer
import prototype.server.core.appointment.appointmentModel.*
import prototype.api.Appointment

trait AppointmentRepository:
  def listAppointments(therapistId: String): IO[List[Appointment]]

final case class AppointmentViewer(
    view: Ref[IO, Map[String, Appointment]]
) extends Viewer[AppointmentState, AppointmentEvent]
    with AppointmentRepository:

  override def evolve(
      state: AppointmentState,
      event: AppointmentEvent
  ): AppointmentState =
    appointmentModel.evolve(state, event)

  override def getState(tag: String): IO[AppointmentState] =
    view.get.map(_.get(tag))

  override def updateState(
      tag: String,
      events: List[AppointmentEvent]
  ): IO[AppointmentState] =
    view
      .updateAndGet(all =>
        val currentState = all.get(tag)
        val newState = events.foldLeft(currentState)(evolve)
        newState match
          case Some(appointment) => all.updated(tag, appointment)
          case None              => all - tag
      )
      .map(_.get(tag))

  override def listAppointments(therapistId: String): IO[List[Appointment]] =
    view.get
      .map(
        _.values
          .filter(_.therapistId.value == therapistId)
          .toList
      )
