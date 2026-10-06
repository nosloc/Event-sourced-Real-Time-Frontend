package prototype.server

import cats.effect.*
import cats.implicits.*
import prototype.api.*

class AppointmentServiceImpl(
    appointments: List[Appointment],
    therapistService: TherapistService[IO]
) extends AppointmentService[IO]:
  override def listTherapistAppointments(
      therapistId: TherapistId
  ): IO[ListTherapistAppointmentsOutput] =
    therapistService.getTherapist(therapistId) *>
      ListTherapistAppointmentsOutput(
        appointments.filter(_.therapistId == therapistId)
      ).pure[IO]
