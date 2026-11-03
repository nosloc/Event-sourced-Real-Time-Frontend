package prototype.shared.therapist

/** A therapist event that does not fit the state it is applied to. Each side
  * decides what it means (the server fails its projection). An exception, so
  * the server can fail with it as it is.
  */
enum TherapistInconsistency(message: String) extends Exception(message):

  /** A creation for a therapist that exists already. */
  case AlreadyExists(event: TherapistEvent)
      extends TherapistInconsistency(
        s"Therapist ${event.id} already exists while handling ${event.getClass.getSimpleName}"
      )
