package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.views.components.SearchBox

object CompaniesTab:
  def apply(
      companies: RemoteList[Company],
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
        itemsS = companies.itemsVar.signal,
        cellsOf = c => List(c.name, addressText(c.address)),
        onSelect = c => selectedVar.set(Some(c)),
        searchS = searchVar.signal,
        errorS = companies.errorVar.signal
      ),
      CreateCompanyModal(showCreateCompanyModalVar, companies),
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
