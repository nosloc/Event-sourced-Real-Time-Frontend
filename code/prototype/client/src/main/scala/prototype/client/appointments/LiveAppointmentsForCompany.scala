package prototype.client.appointments

import prototype.api.*
import prototype.client.Api
import prototype.client.live.{LiveData, LiveFeed, ReplayedLiveData}
import prototype.shared.appointment.*

import com.raquo.laminar.api.L.*

/** A company's appointments, replayed in the browser. The snapshot holds every
  * appointment the events bring (an event does not say which company it belongs
  * to); the value shows only those with one of the company's therapists.
  */
object LiveAppointmentsForCompany:
  def apply(
      feed: LiveFeed,
      companyIdS: Signal[CompanyId],
      companyTherapists: LiveData[List[Therapist]]
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
      .combineWithFn(companyTherapists)((appointments, therapists) =>
        val therapistIds = therapists.map(_.therapistId).toSet
        appointments.filter(_.therapistIds.exists(therapistIds.contains))
      )
