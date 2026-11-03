package prototype.shared.therapist

/** The therapist fold, shared by the server projection and the browser. `Left`
  * when the event does not fit the state (see `TherapistInconsistency`); each
  * side decides what to do with it.
  */
object TherapistEvolve:
  def evolve(
      state: Option[TherapistState],
      event: TherapistEvent
  ): Either[TherapistInconsistency, Option[TherapistState]] =
    (state, event) match
      case (None, created: TherapistCreated) =>
        Right(
          Some(
            TherapistState(
              therapistId = created.id,
              firstName = created.firstName,
              lastName = created.lastName,
              companyId = created.companyId,
              role = created.role.stringValue
            )
          )
        )
      case (Some(_), created: TherapistCreated) =>
        Left(TherapistInconsistency.AlreadyExists(created))
