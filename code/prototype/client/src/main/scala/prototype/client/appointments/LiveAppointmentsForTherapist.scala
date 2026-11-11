package prototype.client.appointments

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.appointment.*

import com.raquo.laminar.api.L.*

object LiveAppointmentsForTherapist:
  def apply(
      feed: LiveFeed,
      therapistIdS: Signal[String]
  ): LiveData[List[Appointment]] =
    new ReplayedLiveData[
      AppointmentOf.Therapist,
      List[Appointment],
      AppointmentEvent
    ](
      feed = feed,
      key = therapistIdS.map(AppointmentOf.Therapist(_)),
      load = therapist =>
        Api.appointments
          .listTherapistAppointments(TherapistId(therapist.trigram))
          .map(response => (response.appointments, response.globalPosition)),
      decode = AppointmentEventDecoder.decode,
      evolve = AppointmentListEvolve.evolve
    )
