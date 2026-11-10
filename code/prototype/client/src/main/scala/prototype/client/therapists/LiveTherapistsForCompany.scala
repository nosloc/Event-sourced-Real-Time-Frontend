package prototype.client.therapists

import prototype.api.{CompanyId, Therapist}
import prototype.client.live.LiveData

import com.raquo.laminar.api.L.*

object LiveTherapistsForCompany:
  def apply(
      therapists: LiveData[List[Therapist]],
      companyIdS: Signal[CompanyId]
  ): LiveData[List[Therapist]] =
    therapists
      .combineWithFn(companyIdS)((all, companyId) =>
        all.filter(_.companyId == companyId)
      )
