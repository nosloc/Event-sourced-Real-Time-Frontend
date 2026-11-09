package prototype.client.companies

import prototype.api.Company
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.company.CompanyEvent

object LiveCompanies:
  def apply(
      feed: LiveFeed
  ): LiveData[List[Company]] =
    ReplayedLiveData.keyless[List[Company], CompanyEvent](
      feed = feed,
      load = Api.companies
        .listCompanies()
        .map(response => (response.companies, response.globalPosition)),
      decode = CompanyEventDecoder.decode,
      evolve = CompanyListEvolve.evolve
    )
