package prototype.server

import cats.effect.*
import cats.implicits.*
import prototype.api.*

class AppointmentServiceImpl(appointments: List[Appointment])
    extends AppointmentService[IO]:
  override def listTherapistAppointments(
      therapistId: TherapistId
  ): IO[ListTherapistAppointmentsOutput] =
    ListTherapistAppointmentsOutput(
      appointments.filter(_.therapistId == therapistId)
    ).pure[IO]
