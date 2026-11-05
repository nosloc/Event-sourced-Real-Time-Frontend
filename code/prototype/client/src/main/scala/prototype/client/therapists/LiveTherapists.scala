package prototype.client.therapists

import prototype.api.Therapist
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.therapist.TherapistEvent

import com.raquo.laminar.api.L.*

object LiveTherapists:
  def apply(
      feed: EventBus[LiveFeed.FeedMessage]
  ): LiveData[List[Therapist]] =
    ReplayedLiveData.keyless[List[Therapist], TherapistEvent](
      feed = feed,
      load = Api.therapists
        .listTherapists()
        .map(response => (response.therapists, response.globalPosition)),
      decode = TherapistEventDecoder.decode,
      evolve = TherapistListEvolve.evolve
    )
