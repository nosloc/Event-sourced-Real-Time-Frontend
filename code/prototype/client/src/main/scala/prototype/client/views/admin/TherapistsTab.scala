package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.views.components.{PopoverPicker, SearchBox}

object TherapistsTab:

  // A therapist with its company, looked up in the shared companies list
  // (None until that list has arrived)
  private final case class TherapistRow(
      therapist: Therapist,
      company: Option[Company]
  ):
    def companyLabel: String =
      company.fold("…")(c => s"${c.name} (${c.address.city})")

  def apply(
      therapists: RemoteList[Therapist],
      companies: RemoteList[Company],
      showCreateTherapistModalVar: Var[Boolean]
  ): HtmlElement =
    // None means every role; the filter is done in the browser
    val roleFilterVar: Var[Option[Role]] = Var(None)
    val searchVar: Var[String] = Var("")
    val selectedVar: Var[Option[TherapistRow]] = Var(None)

    // The role filter first, then the table filters by the search text
    val rowsS: Signal[Option[List[TherapistRow]]] =
      therapists.itemsVar.signal
        .combineWith(roleFilterVar.signal, companies.itemsVar.signal)
        .map { case (items, roleFilter, companyList) =>
          items.map(
            _.filter(t => roleFilter.forall(_ == t.role)).map(t =>
              TherapistRow(
                t,
                companyList.flatMap(_.find(_.companyId == t.companyId))
              )
            )
          )
        }

    div(
      div(
        cls := "admin__tools",
        SearchBox(searchVar, "Search therapists"),
        PopoverPicker[Option[Role]](
          id = "admin-role-filter",
          label = "",
          items = Val(
            PopoverPicker.Item[Option[Role]](None, "All roles") ::
              Role.values.map(r =>
                PopoverPicker.Item[Option[Role]](
                  Some(r),
                  TherapistForm.roleName(r)
                )
              )
          ),
          selectedVar = roleFilterVar,
          fallbackLabel = _ => "All roles"
        )
      ),
      AdminTable[TherapistRow](
        headers = List("Trigram", "Name", "Role", "Company"),
        itemsS = rowsS,
        cellsOf = row =>
          List(
            row.therapist.therapistId.value,
            s"${row.therapist.firstName} ${row.therapist.lastName}",
            TherapistForm.roleName(row.therapist.role),
            row.companyLabel
          ),
        onSelect = row => selectedVar.set(Some(row)),
        searchS = searchVar.signal,
        errorS = therapists.errorVar.signal,
        emptyTextS = roleFilterVar.signal.map(
          _.fold("Nothing here yet.")(_ => "No therapist has this role.")
        )
      ),
      CreateTherapistModal(showCreateTherapistModalVar, therapists, companies),
      DetailsModal[TherapistRow](
        selectedVar,
        "Therapist",
        row =>
          List(
            "Trigram" -> row.therapist.therapistId.value,
            "First name" -> row.therapist.firstName,
            "Last name" -> row.therapist.lastName,
            "Role" -> TherapistForm.roleName(row.therapist.role),
            "Company" -> row.companyLabel,
            "Company id" -> row.therapist.companyId.value.toString
          )
      )
    )
end TherapistsTab
