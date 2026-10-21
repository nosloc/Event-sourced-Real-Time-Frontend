package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import cats.effect.*
import prototype.api.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.components.{FormRow, PopoverPicker}
import prototype.client.ErrorMessages

object TherapistForm:
  // Used to represent the form state on save
  final case class TherapistDraft(
      firstName: String,
      lastName: String,
      therapistId: TherapistId,
      companyId: CompanyId,
      role: Role
  )

  final case class TherapistFormState(
      firstName: String = "",
      lastName: String = "",
      trigram: String = "",
      companyId: Option[CompanyId] = None,
      role: Role = Role.PSYCHOLOGIST,
      showErrors: Boolean = false
  ):
    def firstNameError: Option[String] =
      Option.when(firstName.trim.isEmpty)("Enter a first name")

    def lastNameError: Option[String] =
      Option.when(lastName.trim.isEmpty)("Enter a last name")

    // The same pattern the API enforces on a therapist id
    def trigramError: Option[String] =
      Option.unless(trigram.trim.matches("[a-z]{3}"))(
        "Three lowercase letters, like abc"
      )

    def companyError: Option[String] =
      Option.when(companyId.isEmpty)("Choose a company")

    val toDraft: Option[TherapistDraft] =
      for
        company <- companyId
        if firstNameError.isEmpty && lastNameError.isEmpty &&
          trigramError.isEmpty
      yield TherapistDraft(
        firstName.trim,
        lastName.trim,
        TherapistId(trigram.trim),
        company,
        role
      )

    def displayError(
        error: TherapistFormState => Option[String]
    ): Option[String] =
      error(this).filter(_ => showErrors)
  end TherapistFormState

  def roleName(role: Role): String =
    role.name.toLowerCase.replace("_", " ").capitalize

  def apply[R](
      initialState: TherapistFormState,
      saveFunction: TherapistDraft => IO[R],
      companyListS: Signal[List[Company]],
      onSuccess: () => Unit,
      onCancel: () => Unit
  ): HtmlElement =

    val loadingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)
    val stateVar: Var[TherapistFormState] = Var(initialState)

    def errorS(
        error: TherapistFormState => Option[String]
    ): Signal[Option[String]] =
      stateVar.signal.map(_.displayError(error))

    form(
      onSubmit.preventDefault --> { _ =>
        stateVar.update(_.copy(showErrors = true))
      },
      onSubmit.preventDefault.compose(
        _.sample(stateVar.signal)
          .map(_.toDraft)
          .collect { case Some(draft) => draft }
          .flatMapSwitch { draft =>
            ApiCall.fromIOCallSimple(saveFunction(draft)).stateS
          }
      ) --> {
        case Api.Loading =>
          loadingVar.set(true)
          errorVar.set(None)
        case Api.Success(_) =>
          loadingVar.set(false)
          onSuccess()
        case Api.Error(error) =>
          loadingVar.set(false)
          errorVar.set(Some(ErrorMessages.errorToMessage(error)))
      },
      div(
        cls := "form__pair",
        FormRow.input(
          "First name",
          "text",
          stateVar.zoomLazy(_.firstName)((state, v) =>
            state.copy(firstName = v)
          ),
          errorS(_.firstNameError)
        ),
        FormRow.input(
          "Last name",
          "text",
          stateVar.zoomLazy(_.lastName)((state, v) => state.copy(lastName = v)),
          errorS(_.lastNameError)
        )
      ),
      FormRow.input(
        "Trigram",
        "text",
        stateVar.zoomLazy(_.trigram)((state, v) => state.copy(trigram = v)),
        errorS(_.trigramError)
      ),
      FormRow.pick(
        "Company",
        PopoverPicker[Option[CompanyId]](
          id = "admin-company",
          label = "",
          items = companyListS.map(companies =>
            companies.map(c =>
              PopoverPicker.Item(
                value = Some(c.companyId),
                name = c.name,
                hint = Some(c.address.city),
                buttonLabel = Some(s"${c.name} (${c.address.city})")
              )
            )
          ),
          selectedVar = stateVar.zoomLazy(_.companyId)((state, c) =>
            state.copy(companyId = c)
          ),
          fallbackLabel = _ => "Choose a company"
        ),
        errorS(_.companyError)
      ),
      FormRow.pick(
        "Role",
        PopoverPicker[Role](
          id = "admin-role",
          label = "",
          items = Val(
            Role.values.map(r =>
              PopoverPicker.Item(value = r, name = roleName(r))
            )
          ),
          selectedVar =
            stateVar.zoomLazy(_.role)((state, r) => state.copy(role = r)),
          fallbackLabel = roleName
        ),
        Val(None)
      ),
      child.maybe <-- errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      div(
        cls := "form__actions",
        button(
          typ := "button", // "submit" is the default in HTML
          cls := "btn",
          "Cancel",
          onClick --> { _ => onCancel() }
        ),
        button(
          typ := "submit",
          cls := "btn btn--primary",
          "Create",
          disabled <-- loadingVar.signal
        )
      )
    )
  end apply
end TherapistForm
