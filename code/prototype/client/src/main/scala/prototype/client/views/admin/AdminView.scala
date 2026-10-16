package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.client.Api

object AdminView:

  private enum Section(val label: String, val newLabel: String):
    case Companies extends Section("Companies", "New company")
    case Therapists extends Section("Therapists", "New therapist")
    case Patients extends Section("Patients", "New patient")

  def apply(): HtmlElement =
    val sectionVar: Var[Section] = Var(Section.Companies)
    val showCreateCompanyVar: Var[Boolean] = Var(false)
    val showCreateTherapistVar: Var[Boolean] = Var(false)
    val showCreatePatientVar: Var[Boolean] = Var(false)

    val companies = RemoteList(Api.companies.listCompanies().map(_.companies))
    val therapists =
      RemoteList(Api.therapists.listTherapists().map(_.therapists))
    val patients = RemoteList(Api.patients.listPatients().map(_.patients))

    div(
      cls := "admin",
      companies.keepLoaded,
      therapists.keepLoaded,
      patients.keepLoaded,
      h1("Administration"),
      div(
        cls := "admin__bar",
        renderTabs(sectionVar),
        button(
          typ := "button",
          cls := "btn btn--primary",
          child.text <-- sectionVar.signal.map(_.newLabel),
          onClick.compose(_.sample(sectionVar.signal)) --> {
            case Section.Companies  => showCreateCompanyVar.set(true)
            case Section.Therapists => showCreateTherapistVar.set(true)
            case Section.Patients   => showCreatePatientVar.set(true)
          }
        )
      ),
      child <-- sectionVar.signal.map {
        case Section.Companies =>
          CompaniesTab(companies, showCreateCompanyVar)
        case Section.Therapists =>
          TherapistsTab(therapists, companies, showCreateTherapistVar)
        case Section.Patients =>
          PatientsTab(patients, showCreatePatientVar)
      }
    )
  end apply

  private def renderTabs(sectionVar: Var[Section]): HtmlElement =
    div(
      cls := "admin__tabs",
      Section.values.toList.map(section =>
        button(
          typ := "button",
          cls := "admin__tab",
          cls("admin__tab--active") <-- sectionVar.signal.map(_ == section),
          section.label,
          onClick --> { _ => sectionVar.set(section) }
        )
      )
    )
end AdminView
