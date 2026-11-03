package prototype.shared.patient

/** The patient fold, shared by the server projection and the browser. `Left`
  * when the event does not fit the state (see `PatientInconsistency`); each side
  * decides what to do with it.
  */
object PatientEvolve:
  def evolve(
      state: Option[PatientState],
      event: PatientEvent
  ): Either[PatientInconsistency, Option[PatientState]] =
    (state, event) match
      case (None, created: PatientCreated) =>
        Right(
          Some(
            PatientState(
              patientId = created.id,
              firstName = created.firstName,
              lastName = created.lastName,
              dateOfBirth = created.dateOfBirth
            )
          )
        )
      case (Some(_), created: PatientCreated) =>
        Left(PatientInconsistency.AlreadyExists(created))
