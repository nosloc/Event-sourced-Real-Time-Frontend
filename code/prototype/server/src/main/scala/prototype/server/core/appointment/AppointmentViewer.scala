package prototype.server.core.appointment

import cats.effect.*
import prototype.server.eventStore.Viewer
import prototype.server.core.appointment.appointmentModel.*
import prototype.api.{Appointment, PatientId, TherapistId}

trait AppointmentRepository:
  def listAppointments(therapistId: TherapistId): IO[List[Appointment]]
  def listAppointmentsForTherapists(
      therapistIds: List[TherapistId]
  ): IO[List[Appointment]]
  def listAppointmentsForPatients(
      patientIds: List[PatientId]
  ): IO[List[Appointment]]

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

  override def listAppointments(
      therapistId: TherapistId
  ): IO[List[Appointment]] =
    view.get
      .map(
        _.values
          .filter(_.therapistIds.contains(therapistId))
          .toList
      )
  end listAppointments

  // Appointments with at least one of these therapists
  override def listAppointmentsForTherapists(
      therapistIds: List[TherapistId]
  ): IO[List[Appointment]] =
    view.get.map(
      _.values.filter(_.therapistIds.exists(therapistIds.contains)).toList
    )

  // Appointments with at least one of these patients
  override def listAppointmentsForPatients(
      patientIds: List[PatientId]
  ): IO[List[Appointment]] =
    view.get.map(
      _.values.filter(_.patientIds.exists(patientIds.contains)).toList
    )
