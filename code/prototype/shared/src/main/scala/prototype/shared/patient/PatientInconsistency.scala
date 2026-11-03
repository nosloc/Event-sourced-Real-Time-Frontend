package prototype.shared.patient

/** A patient event that does not fit the state it is applied to. Each side
  * decides what it means (the server fails its projection). An exception, so
  * the server can fail with it as it is.
  */
enum PatientInconsistency(message: String) extends Exception(message):

  /** A creation for a patient that exists already. */
  case AlreadyExists(event: PatientEvent)
      extends PatientInconsistency(
        s"Patient ${event.id} already exists while handling ${event.getClass.getSimpleName}"
      )
