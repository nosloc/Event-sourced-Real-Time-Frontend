package prototype.server.core.therapist.domain

import prototype.server.UserFacingFailure

enum TherapistError(val message: String)
    extends Exception(message),
      UserFacingFailure:

  case THERAPIST_ALREADY_EXISTS(therapistId: String)
      extends TherapistError(s"Therapist $therapistId already exists.")

  case THERAPIST_NAME_EMPTY(therapistId: String)
      extends TherapistError(s"Therapist $therapistId has empty name.")

  case THERAPIST_COMPANY_NOT_FOUND(therapistId: String)
      extends TherapistError(
        s"Therapist $therapistId has non-existing company."
      )
