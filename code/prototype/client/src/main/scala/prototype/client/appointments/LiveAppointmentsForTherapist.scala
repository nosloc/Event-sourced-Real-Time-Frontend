package prototype.client.appointments

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData, Same}
import prototype.shared.appointment.*

import com.raquo.laminar.api.L.*

object LiveAppointmentsForTherapist:
  // Its name in the trace
  val name: String = "therapist appointments"

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
      name = name,
      key = therapistIdS.map(AppointmentOf.Therapist(_)),
      load = therapist =>
        Api.appointments
          .listTherapistAppointments(TherapistId(therapist.trigram))
          .map(response => (response.appointments, response.cursor)),
      decode = AppointmentEventDecoder.decode,
      evolve = AppointmentListEvolve.evolve,
      same = Same.byId(_.appointmentId)
    )
