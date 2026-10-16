package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api
import prototype.client.views.admin.CompanyForm.CompanyFormState
import prototype.client.views.components.Modal

object CreateCompanyModal:
  def apply(
      showCreateCompanyModalVar: Var[Boolean],
      companies: RemoteList[Company]
  ): Mod[HtmlElement] =
    child.maybe <-- showCreateCompanyModalVar.signal.map(isOpen =>
      Option.when(isOpen) {
        // Fresh state each time the modal opens
        Modal(
          title = "New company",
          onClose = () => showCreateCompanyModalVar.set(false)
        )(
          CompanyForm(
            initialState = CompanyFormState(),
            saveFunction = draft =>
              Api.companies.createCompany(draft.name, draft.address),
            onSuccess = () => {
              showCreateCompanyModalVar.set(false)
              companies.reload()
            },
            onCancel = () => showCreateCompanyModalVar.set(false)
          )
        )
      }
    )
end CreateCompanyModal
