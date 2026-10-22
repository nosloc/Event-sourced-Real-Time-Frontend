package prototype.server.core.appointment

import cats.effect.*

import prototype.api.*

/** An appointment service that keeps its appointments in memory, for the tests
  * of the services that read appointments (dashboards). It checks none of the
  * appointment rules (busy people, time range, status changes): those are
  * tested on the real handlers. Like the real one, it lists only the
  * appointments of a known therapist, in time order.
  */
final class InMemoryAppointmentService private (
    therapists: TherapistService[IO],
    appointments: Ref[IO, Map[AppointmentId, Appointment]]
) extends AppointmentService[IO]:

  private def change(id: AppointmentId)(f: Appointment => Appointment): IO[Appointment] =
    appointments
      .modify(all =>
        all.get(id) match
          case Some(found) =>
            val changed = f(found)
            (all + (id -> changed), Some(changed))
          case None => (all, None)
      )
      .flatMap(IO.fromOption(_)(AppointmentNotFound()))

  override def createAppointment(
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
      apptType: AppointmentType,
      timeRange: TimeRange
  ): IO[CreateAppointmentOutput] =
    IO.randomUUID.flatMap { uuid =>
      val appointment = Appointment(
        AppointmentId(uuid),
        therapistIds,
        patientIds,
        apptType,
        timeRange,
        Nil
      )
      appointments
        .update(_ + (appointment.appointmentId -> appointment))
        .as(CreateAppointmentOutput(appointment))
    }

  override def getAppointment(appointmentId: AppointmentId): IO[GetAppointmentOutput] =
    appointments.get
      .map(_.get(appointmentId))
      .flatMap(IO.fromOption(_)(AppointmentNotFound()))
      .map(GetAppointmentOutput.apply)

  override def listTherapistAppointments(
      therapistId: TherapistId
  ): IO[ListTherapistAppointmentsOutput] =
    therapists.getTherapist(therapistId) *>
      appointments.get.map(all =>
        ListTherapistAppointmentsOutput(
          all.values.toList
            .filter(_.therapistIds.contains(therapistId))
            .sortBy(_.timeRange.start.epochSecond)
        )
      )

  override def updateAppointment(
      appointmentId: AppointmentId,
      apptType: AppointmentType,
      therapistIds: List[TherapistId],
      patientIds: List[PatientId],
      timeRange: TimeRange
  ): IO[UpdateAppointmentOutput] =
    change(appointmentId)(
      _.copy(
        therapistIds = therapistIds,
        patientIds = patientIds,
        appointmentType = apptType,
        timeRange = timeRange
      )
    ).map(UpdateAppointmentOutput.apply)

  override def deleteAppointment(appointmentId: AppointmentId): IO[Unit] =
    appointments.update(_ - appointmentId)

  override def createPrestation(
      appointmentId: AppointmentId,
      label: String,
      quantity: Int,
      unitPrice: Float
  ): IO[CreatePrestationOutput] =
    IO.randomUUID.flatMap { uuid =>
      val prestation = Prestation(
        appointmentId,
        PrestationId(uuid),
        label,
        quantity,
        unitPrice,
        PrestationStatus.TO_BILL
      )
      change(appointmentId)(a => a.copy(prestations = a.prestations :+ prestation))
        .map(CreatePrestationOutput.apply)
    }

  override def updatePrestationStatus(
      appointmentId: AppointmentId,
      prestationId: PrestationId,
      status: PrestationStatus
  ): IO[UpdatePrestationStatusOutput] =
    change(appointmentId)(a =>
      a.copy(prestations =
        a.prestations.map(p =>
          if p.prestationId == prestationId then p.copy(status = status) else p
        )
      )
    ).map(UpdatePrestationStatusOutput.apply)

  override def deletePrestation(
      appointmentId: AppointmentId,
      prestationId: PrestationId
  ): IO[Unit] =
    change(appointmentId)(a =>
      a.copy(prestations = a.prestations.filterNot(_.prestationId == prestationId))
    ).void

object InMemoryAppointmentService:
  def apply(therapists: TherapistService[IO]): IO[InMemoryAppointmentService] =
    Ref
      .of[IO, Map[AppointmentId, Appointment]](Map.empty)
      .map(new InMemoryAppointmentService(therapists, _))
