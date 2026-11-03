package prototype.shared.company

/** A company event that does not fit the state it is applied to. Each side
  * decides what it means (the server fails its projection). An exception, so
  * the server can fail with it as it is.
  */
enum CompanyInconsistency(message: String) extends Exception(message):

  /** A creation for a company that exists already. */
  case AlreadyExists(event: CompanyEvent)
      extends CompanyInconsistency(
        s"Company ${event.id} already exists while handling ${event.getClass.getSimpleName}"
      )
