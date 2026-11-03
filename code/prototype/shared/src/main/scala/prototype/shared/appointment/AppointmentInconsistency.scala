package prototype.shared.appointment

/** An appointment event that does not fit the state it is applied to. Each side
  * decides what it means: the server holds every appointment, so it is an error
  * there; the browser holds one therapist's appointments, so a missing one is
  * normal. An exception, so the server can fail with it as it is.
  */
enum AppointmentInconsistency(message: String) extends Exception(message):

  /** A creation for an appointment that exists already. */
  case AlreadyExists(event: AppointmentEvent)
      extends AppointmentInconsistency(
        s"Appointment ${event.id} already exists while handling ${event.getClass.getSimpleName}"
      )

  /** A change to an appointment that does not exist. */
  case NotFound(event: AppointmentEvent)
      extends AppointmentInconsistency(
        s"Appointment ${event.id} does not exist while handling ${event.getClass.getSimpleName}"
      )
