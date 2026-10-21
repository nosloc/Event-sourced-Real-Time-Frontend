package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import cats.effect.*
import prototype.api.Address
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.components.FormRow
import prototype.client.ErrorMessages

object CompanyForm:
  // Used to represent the form state on save
  final case class CompanyDraft(name: String, address: Address)

  final case class CompanyFormState(
      name: String = "",
      street: String = "",
      number: String = "",
      city: String = "",
      state: String = "",
      showErrors: Boolean = false
  ):
    private def required(value: String, message: String): Option[String] =
      Option.when(value.trim.isEmpty)(message)

    def nameError: Option[String] = required(name, "Enter a name")
    def streetError: Option[String] = required(street, "Enter a street")
    def numberError: Option[String] = required(number, "Enter a number")
    def cityError: Option[String] = required(city, "Enter a city")
    def stateError: Option[String] = required(state, "Enter a state")

    val toDraft: Option[CompanyDraft] =
      Option.when(
        List(nameError, streetError, numberError, cityError, stateError)
          .forall(_.isEmpty)
      )(
        CompanyDraft(
          name.trim,
          Address(street.trim, number.trim, city.trim, state.trim)
        )
      )

    def displayError(
        error: CompanyFormState => Option[String]
    ): Option[String] =
      error(this).filter(_ => showErrors)

  def apply[R](
      initialState: CompanyFormState,
      saveFunction: CompanyDraft => IO[R],
      onSuccess: () => Unit,
      onCancel: () => Unit
  ): HtmlElement =

    val loadingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)
    val stateVar: Var[CompanyFormState] = Var(initialState)

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
      renderRow(
        "Name",
        stateVar,
        _.name,
        (s, v) => s.copy(name = v),
        _.nameError
      ),
      // Street and number side by side, then city and state
      div(
        cls := "form__pair",
        renderRow(
          "Street",
          stateVar,
          _.street,
          (s, v) => s.copy(street = v),
          _.streetError
        ),
        renderRow(
          "Number",
          stateVar,
          _.number,
          (s, v) => s.copy(number = v),
          _.numberError
        )
      ),
      div(
        cls := "form__pair",
        renderRow(
          "City",
          stateVar,
          _.city,
          (s, v) => s.copy(city = v),
          _.cityError
        ),
        renderRow(
          "State",
          stateVar,
          _.state,
          (s, v) => s.copy(state = v),
          _.stateError
        )
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

  // Errors only show after the first submit (see displayError)
  private def renderRow(
      labelText: String,
      stateVar: Var[CompanyFormState],
      get: CompanyFormState => String,
      set: (CompanyFormState, String) => CompanyFormState,
      error: CompanyFormState => Option[String]
  ): HtmlElement =
    FormRow.input(
      labelText,
      "text",
      stateVar.zoomLazy(get)(set),
      stateVar.signal.map(_.displayError(error))
    )
end CompanyForm
