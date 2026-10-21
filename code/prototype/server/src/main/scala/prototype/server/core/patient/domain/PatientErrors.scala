package prototype.server.core.patient.domain

import java.util.UUID

import prototype.server.UserFacingFailure

enum PatientError(val message: String)
    extends Exception(message),
      UserFacingFailure:

  case PATIENT_ALREADY_EXISTS(id: UUID)
      extends PatientError(s"Patient $id already exists.")
  case EMPTY_PATIENT_FIELDS(fieldNames: List[String])
      extends PatientError(
        s"These patient fields are empty: ${fieldNames.mkString(", ")}."
      )
