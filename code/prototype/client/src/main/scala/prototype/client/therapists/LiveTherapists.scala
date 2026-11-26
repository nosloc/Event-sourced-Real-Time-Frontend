package prototype.client.therapists

import prototype.api.Therapist
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData, Same}
import prototype.shared.therapist.TherapistEvent

object LiveTherapists:
  // Its name in the trace
  val name: String = "therapists"

  def apply(
      feed: LiveFeed
  ): LiveData[List[Therapist]] =
    ReplayedLiveData.keyless[List[Therapist], TherapistEvent](
      feed = feed,
      name = name,
      load = Api.therapists
        .listTherapists()
        .map(response => (response.therapists, response.catchUp)),
      decode = TherapistEventDecoder.decode,
      evolve = TherapistListEvolve.evolve,
      same = Same.byId(_.therapistId)
    )
