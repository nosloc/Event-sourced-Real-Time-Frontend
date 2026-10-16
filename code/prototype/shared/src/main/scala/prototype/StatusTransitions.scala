package prototype

import prototype.api.PrestationStatus

object StatusTransitions:
  def verifyStatusChange(
      currentStatus: PrestationStatus,
      newStatus: PrestationStatus
  ): Boolean =
    (currentStatus, newStatus) match
      case (PrestationStatus.TO_BILL, PrestationStatus.BILLED)    => true
      case (PrestationStatus.BILLED, PrestationStatus.PAID)       => true
      case (PrestationStatus.BILLED, PrestationStatus.CANCELLED)  => true
      case (PrestationStatus.TO_BILL, PrestationStatus.CANCELLED) => true
      case _                                                      => false
