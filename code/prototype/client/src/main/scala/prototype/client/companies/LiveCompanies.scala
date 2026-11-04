package prototype.client.companies

import prototype.api.Company
import prototype.client.Api
import prototype.client.live.{
  LiveDataSource,
  LiveFeed
}
import prototype.shared.company.CompanyEvent

import com.raquo.laminar.api.L.*

object LiveCompanies:
  def apply(
      feed: EventBus[LiveFeed.FeedMessage]
  ): LiveDataSource[Unit, List[Company], CompanyEvent] =
    new LiveDataSource[Unit, List[Company], CompanyEvent](
      feed = feed,
      key = Signal.fromValue(()),
      load = _ =>
        Api.companies
          .listCompanies()
          .map(response => (response.companies, response.globalPosition)),
      decode = CompanyEventDecoder.decode,
      evolve = (_, companies, event) =>
        CompanyListEvolve.evolve(companies, event)
    )
