package prototype.client.views.admin

import prototype.api.*
import prototype.client.ErrorMessages
import prototype.client.views.components.SearchBox

import com.raquo.laminar.api.L.*

object CompaniesTab:
  def apply(
      companies: Signal[Option[List[Company]]],
      errorS: Signal[Option[Throwable]],
      showCreateCompanyModalVar: Var[Boolean]
  ): HtmlElement =
    val searchVar: Var[String] = Var("")
    val selectedVar: Var[Option[Company]] = Var(None)

    div(
      div(
        cls := "admin__tools",
        SearchBox(searchVar, "Search companies")
      ),
      AdminTable[Company](
        headers = List("Name", "Address"),
        itemsS = companies,
        cellsOf = c => List(c.name, addressText(c.address)),
        onSelect = c => selectedVar.set(Some(c)),
        searchS = searchVar.signal,
        errorS = errorS.map(_.map(ErrorMessages.errorToMessage))
      ),
      CreateCompanyModal(showCreateCompanyModalVar),
      DetailsModal[Company](
        selectedVar,
        "Company",
        c =>
          List(
            "Id" -> c.companyId.value.toString,
            "Name" -> c.name,
            "Street" -> s"${c.address.street} ${c.address.number}",
            "City" -> c.address.city,
            "State" -> c.address.state
          )
      )
    )

  private def addressText(a: Address): String =
    s"${a.street} ${a.number}, ${a.city} (${a.state})"
end CompaniesTab
