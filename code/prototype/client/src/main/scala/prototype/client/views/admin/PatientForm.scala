package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import cats.effect.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.components.FormRow
import prototype.client.ErrorMessages
import smithy4s.time.LocalDate
import scala.util.Try

object PatientForm:
  // Used to represent the form state on save
  final case class PatientDraft(
      firstName: String,
      lastName: String,
      dateOfBirth: LocalDate
  )

  final case class PatientFormState(
      firstName: String = "",
      lastName: String = "",
      dateOfBirth: String = "",
      showErrors: Boolean = false
  ):
    def firstNameError: Option[String] =
      Option.when(firstName.trim.isEmpty)("Enter a first name")

    def lastNameError: Option[String] =
      Option.when(lastName.trim.isEmpty)("Enter a last name")

    // A date input gives yyyy-mm-dd, or an empty text when it is not a date
    private def parsedDate: Option[LocalDate] =
      dateOfBirth.split("-").toList.map(_.toIntOption) match
        case List(Some(y), Some(m), Some(d)) =>
          Try(LocalDate(y, m, d)).toOption
        case _ => None

    def dateOfBirthError: Option[String] =
      Option.when(parsedDate.isEmpty)("Pick a date of birth")

    val toDraft: Option[PatientDraft] =
      for date <- parsedDate
      yield PatientDraft(firstName.trim, lastName.trim, date)

    def displayError(
        error: PatientFormState => Option[String]
    ): Option[String] =
      error(this).filter(_ => showErrors)

  def apply[R](
      initialState: PatientFormState,
      saveFunction: PatientDraft => IO[R],
      onSuccess: () => Unit,
      onCancel: () => Unit
  ): HtmlElement =

    val loadingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)
    val stateVar: Var[PatientFormState] = Var(initialState)

    def errorS(
        error: PatientFormState => Option[String]
    ): Signal[Option[String]] =
      stateVar.signal.map(_.displayError(error))

    form(
      onSubmit.preventDefault --> { _ =>
        stateVar.update(_.copy(showErrors = true))
      },
      onSubmit.preventDefault.compose(
        _.sample(stateVar.signal)
          // The names are checked too, not only the date
          .filter(s => s.firstNameError.isEmpty && s.lastNameError.isEmpty)
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
        "Date of birth",
        "date",
        stateVar.zoomLazy(_.dateOfBirth)((state, v) =>
          state.copy(dateOfBirth = v)
        ),
        errorS(_.dateOfBirthError)
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
end PatientForm
