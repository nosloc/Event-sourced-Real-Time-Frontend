package prototype.client.appointments

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.appointment.*

import com.raquo.laminar.api.L.*

object LiveAppointmentsForCompany:
  def apply(
      feed: LiveFeed,
      companyIdS: Signal[CompanyId]
  ): LiveData[List[Appointment]] =
    new ReplayedLiveData[
      AppointmentOf.Company,
      List[Appointment],
      AppointmentEvent
    ](
      feed = feed,
      key = companyIdS.map(id => AppointmentOf.Company(id.value)),
      load = company =>
        Api.appointments
          .listCompanyAppointments(CompanyId(company.companyId))
          .map(response => (response.appointments, response.globalPosition)),
      decode = AppointmentEventDecoder.decode,
      evolve = AppointmentListEvolve.evolve
    )
