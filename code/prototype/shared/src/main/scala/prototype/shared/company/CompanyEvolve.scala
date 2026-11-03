package prototype.shared.company

object CompanyEvolve:
  def evolve(
      state: Option[CompanyState],
      event: CompanyEvent
  ): Either[CompanyInconsistency, Option[CompanyState]] =
    (state, event) match
      case (None, created: CompanyCreated) =>
        Right(
          Some(
            CompanyState(
              companyId = created.id,
              name = created.name,
              addressStreet = created.addressStreet,
              addressNumber = created.addressNumber,
              addressCity = created.addressCity,
              addressState = created.addressState
            )
          )
        )
      case (Some(_), _: CompanyCreated) =>
        Left(CompanyInconsistency.AlreadyExists(event))
