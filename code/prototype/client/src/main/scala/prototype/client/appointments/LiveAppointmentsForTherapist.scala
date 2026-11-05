package prototype.client.appointments

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.appointment.*

import com.raquo.laminar.api.L.*

object LiveAppointmentsForTherapist:
  def apply(
      feed: EventBus[LiveFeed.FeedMessage],
      therapistIdS: Signal[String]
  ): LiveData[List[Appointment]] =
    new ReplayedLiveData[String, List[Appointment], AppointmentEvent](
      feed = feed,
      key = therapistIdS,
      load = therapistId =>
        Api.appointments
          .listTherapistAppointments(TherapistId(therapistId))
          .map(response => (response.appointments, response.globalPosition)),
      decode = AppointmentEventDecoder.decode,
      evolve = AppointmentListEvolve.evolve
    )
