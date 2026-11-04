package prototype.client.views.admin

import prototype.api.*
import prototype.client.ErrorMessages
import prototype.client.views.components.SearchBox

import com.raquo.laminar.api.L.*

object PatientsTab:
  def apply(
      patientsS: Signal[Option[List[Patient]]],
      errorS: Signal[Option[Throwable]],
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
        itemsS = patientsS,
        cellsOf = p => List(p.lastName, p.firstName, p.dateOfBirth.toString),
        onSelect = p => selectedVar.set(Some(p)),
        searchS = searchVar.signal,
        errorS = errorS.map(_.map(ErrorMessages.errorToMessage))
      ),
      CreatePatientModal(showCreatePatientModalVar),
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
