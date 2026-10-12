package prototype.server.core.appointment

import cats.effect.*
import prototype.api.*

trait AppointmentServiceImpl extends AppointmentService[IO]

object AppointmentServiceImpl:
  def apply(
      // eventStore: EventStore[AppointmentEvent]
  ): IO[AppointmentServiceImpl] =
    for
      view <- Ref.of[IO, Map[String, Appointment]](Map.empty)
      viewer = AppointmentViewer(view)
    // decider = AppointmentDecider()
    yield new AppointmentServiceImpl:

      override def listTherapistAppointments(
          therapistId: TherapistId
      ): IO[ListTherapistAppointmentsOutput] =
        viewer
          .listAppointments(therapistId.value)
          .map(ListTherapistAppointmentsOutput.apply)

      // val handler = new CommandHandler(decider, viewer, eventStore)
