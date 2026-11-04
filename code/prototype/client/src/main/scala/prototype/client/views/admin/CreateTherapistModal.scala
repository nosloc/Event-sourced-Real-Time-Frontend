package prototype.client.views.admin

import prototype.api.*
import prototype.client.Api
import prototype.client.views.admin.TherapistForm.TherapistFormState
import prototype.client.views.components.Modal

import com.raquo.laminar.api.L.*

object CreateTherapistModal:
  def apply(
      showCreateTherapistModalVar: Var[Boolean],
      companies: Signal[List[Company]]
  ): Mod[HtmlElement] =
    child.maybe <-- showCreateTherapistModalVar.signal.map(isOpen =>
      Option.when(isOpen) {
        // Fresh state each time the modal opens
        Modal(
          title = "New therapist",
          onClose = () => showCreateTherapistModalVar.set(false)
        )(
          TherapistForm(
            initialState = TherapistFormState(),
            saveFunction = draft =>
              Api.therapists.createTherapist(
                draft.firstName,
                draft.lastName,
                draft.therapistId,
                draft.companyId,
                draft.role
              ),
            companyListS = companies,
            onSuccess = () => {
              showCreateTherapistModalVar.set(false)
            },
            onCancel = () => showCreateTherapistModalVar.set(false)
          )
        )
      }
    )
end CreateTherapistModal
