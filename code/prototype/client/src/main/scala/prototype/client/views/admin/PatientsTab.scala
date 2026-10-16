package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.views.components.SearchBox

object PatientsTab:
  def apply(
      patients: RemoteList[Patient],
      showCreatePatientModalVar: Var[Boolean]
  ): HtmlElement =
    val searchVar: Var[String] = Var("")
    val selectedVar: Var[Option[Patient]] = Var(None)

    div(
      div(
        cls := "admin__tools",
        SearchBox(searchVar, "Search patients")
      ),
      AdminTable[Patient](
        headers = List("Last name", "First name", "Date of birth"),
        itemsS = patients.itemsVar.signal,
        cellsOf = p => List(p.lastName, p.firstName, p.dateOfBirth.toString),
        onSelect = p => selectedVar.set(Some(p)),
        searchS = searchVar.signal,
        errorS = patients.errorVar.signal
      ),
      CreatePatientModal(showCreatePatientModalVar, patients),
      DetailsModal[Patient](
        selectedVar,
        "Patient",
        p =>
          List(
            "Id" -> p.id.value.toString,
            "First name" -> p.firstName,
            "Last name" -> p.lastName,
            "Date of birth" -> p.dateOfBirth.toString
          )
      )
    )
end PatientsTab
