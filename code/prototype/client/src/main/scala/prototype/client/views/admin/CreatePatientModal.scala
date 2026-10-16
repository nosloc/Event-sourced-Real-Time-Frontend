package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api
import prototype.client.views.admin.PatientForm.PatientFormState
import prototype.client.views.components.Modal

object CreatePatientModal:
  def apply(
      showCreatePatientModalVar: Var[Boolean],
      patients: RemoteList[Patient]
  ): Mod[HtmlElement] =
    child.maybe <-- showCreatePatientModalVar.signal.map(isOpen =>
      Option.when(isOpen) {
        // Fresh state each time the modal opens
        Modal(
          title = "New patient",
          onClose = () => showCreatePatientModalVar.set(false)
        )(
          PatientForm(
            initialState = PatientFormState(),
            saveFunction = draft =>
              Api.patients.createPatient(
                draft.firstName,
                draft.lastName,
                draft.dateOfBirth
              ),
            onSuccess = () => {
              showCreatePatientModalVar.set(false)
              patients.reload()
            },
            onCancel = () => showCreatePatientModalVar.set(false)
          )
        )
      }
    )
end CreatePatientModal
