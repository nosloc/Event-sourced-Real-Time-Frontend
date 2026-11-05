package prototype.client.patients

import prototype.api.Patient
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.patient.PatientEvent

import com.raquo.laminar.api.L.*

object LivePatients:
  def apply(
      feed: EventBus[LiveFeed.FeedMessage]
  ): LiveData[List[Patient]] =
    ReplayedLiveData.keyless[List[Patient], PatientEvent](
      feed = feed,
      load = Api.patients
        .listPatients()
        .map(response => (response.patients, response.globalPosition)),
      decode = PatientEventDecoder.decode,
      evolve = PatientListEvolve.evolve
    )
