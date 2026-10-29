package prototype.client.appointments

import com.raquo.laminar.api.L.*
import prototype.client.live.LiveDataSource
import prototype.api.*
import prototype.client.live.LiveFeed
import prototype.client.Api

object LiveAppointmentsForTherapist:
  def apply(
      feed: EventBus[LiveFeed.FeedMessage],
      therapistIdS: Signal[String]
  ): LiveDataSource[String, List[Appointment], AppointmentEvent] =
    new LiveDataSource[String, List[Appointment], AppointmentEvent](
      feed = feed,
      key = therapistIdS,
      load = therapistId =>
        Api.appointments
          .listTherapistAppointments(TherapistId(therapistId))
          .map(response => (response.appointments, response.globalPosition)),
      decode = AppointmentEvent.decode,
      evolve = AppointmentEvolve.evolve
    )
