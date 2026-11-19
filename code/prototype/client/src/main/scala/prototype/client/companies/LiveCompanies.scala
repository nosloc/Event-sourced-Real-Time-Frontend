package prototype.client.companies

import prototype.api.Company
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData, Same}
import prototype.shared.company.CompanyEvent

object LiveCompanies:
  // Its name in the trace
  val name: String = "companies"

  def apply(
      feed: LiveFeed
  ): LiveData[List[Company]] =
    ReplayedLiveData.keyless[List[Company], CompanyEvent](
      feed = feed,
      name = name,
      load = Api.companies
        .listCompanies()
        .map(response => (response.companies, response.globalPosition)),
      decode = CompanyEventDecoder.decode,
      evolve = CompanyListEvolve.evolve,
      same = Same.byId(_.companyId)
    )
