package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api
import prototype.client.views.admin.TherapistForm.TherapistFormState
import prototype.client.views.components.Modal

object CreateTherapistModal:
  def apply(
      showCreateTherapistModalVar: Var[Boolean],
      therapists: RemoteList[Therapist],
      companies: RemoteList[Company]
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
            companyListS = companies.itemsVar.signal.map(_.getOrElse(Nil)),
            onSuccess = () => {
              showCreateTherapistModalVar.set(false)
              therapists.reload()
            },
            onCancel = () => showCreateTherapistModalVar.set(false)
          )
        )
      }
    )
end CreateTherapistModal
