package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.DemoDataAlreadyGenerated
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.ErrorMessages
import prototype.client.views.admin.DemoDataModal.Outcome

object AdminView:

  private enum Section(val label: String, val newLabel: String):
    case Companies extends Section("Companies", "New company")
    case Therapists extends Section("Therapists", "New therapist")
    case Patients extends Section("Patients", "New patient")

  // The state of "Generate demo data": the lists reload only once the server
  // has answered
  private final class DemoData:
    val generatingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)
    val outcomeVar: Var[Option[Outcome]] = Var(None)
    // Generating, or showing the results
    val busyS: Signal[Boolean] = generatingVar.signal
      .combineWith(outcomeVar.signal)
      .map((generating, outcome) => generating || outcome.isDefined)

  // The account the server creates at boot (DevAccount)
  private val DevTherapistId = "dev"

  def apply(): HtmlElement =
    val sectionVar: Var[Section] = Var(Section.Companies)
    val showCreateCompanyVar: Var[Boolean] = Var(false)
    val showCreateTherapistVar: Var[Boolean] = Var(false)
    val showCreatePatientVar: Var[Boolean] = Var(false)

    val companies = RemoteList(Api.companies.listCompanies().map(_.companies))
    val therapists =
      RemoteList(Api.therapists.listTherapists().map(_.therapists))
    val patients = RemoteList(Api.patients.listPatients().map(_.patients))

    val demoData = DemoData()
    def reloadAll(): Unit =
      companies.reload()
      therapists.reload()
      patients.reload()

    // Same rule as the server: only the dev account exists. Hidden until the
    // therapists have loaded, so it never flashes on a page that has data
    val onlyDevS: Signal[Boolean] = therapists.itemsVar.signal.map(
      _.exists(_.forall(_.therapistId.value == DevTherapistId))
    )
    // Kept on screen (disabled) while it runs and while the results are shown,
    // even once the reloaded lists hold the new data. Distinct: a signal emits
    // again on an equal value, which would rebuild the button and drop the
    // request that lives on it
    val showButtonS: Signal[Boolean] = onlyDevS
      .combineWith(demoData.busyS)
      .map(_ || _)
      .distinct

    div(
      cls := "admin",
      companies.keepLoaded,
      therapists.keepLoaded,
      patients.keepLoaded,
      renderHead(demoData, showButtonS, () => reloadAll()),
      child.maybe <-- demoData.errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      DemoDataModal(demoData.outcomeVar),
      renderBar(
        sectionVar,
        {
          case Section.Companies  => showCreateCompanyVar.set(true)
          case Section.Therapists => showCreateTherapistVar.set(true)
          case Section.Patients   => showCreatePatientVar.set(true)
        }
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

  private def renderHead(
      demoData: DemoData,
      showButtonS: Signal[Boolean],
      reloadAll: () => Unit
  ): HtmlElement =
    div(
      cls := "admin__head",
      h1("Administration"),
      child.maybe <-- showButtonS.map(
        Option.when(_)(renderGenerateButton(demoData, reloadAll))
      )
    )

  // The button stays on screen until the run is over, so the request can live
  // on it
  private def renderGenerateButton(
      demoData: DemoData,
      reloadAll: () => Unit
  ): HtmlElement =
    button(
      typ := "button",
      cls := "btn btn--primary",
      disabled <-- demoData.busyS,
      child.text <-- demoData.generatingVar.signal.map(
        if _ then "Generating…" else "Generate demo data"
      ),
      onClick.compose(
        _.flatMapSwitch(_ =>
          ApiCall.fromIOCallSimple(Api.demoData.generateDemoData()).stateS
        )
      ) --> {
        case Api.Loading =>
          demoData.generatingVar.set(true)
          demoData.errorVar.set(None)
        case Api.Success(output) =>
          demoData.generatingVar.set(false)
          reloadAll()
          demoData.outcomeVar.set(Some(Outcome.Generated(output)))
        case Api.Error(DemoDataAlreadyGenerated()) =>
          demoData.generatingVar.set(false)
          reloadAll()
          demoData.outcomeVar.set(Some(Outcome.AlreadyGenerated))
        case Api.Error(error) =>
          demoData.generatingVar.set(false)
          // A failed run may have written part of the data
          reloadAll()
          demoData.errorVar.set(Some(ErrorMessages.errorToMessage(error)))
      }
    )

  // The section tabs, and the "New …" button of the open section
  private def renderBar(
      sectionVar: Var[Section],
      openCreate: Section => Unit
  ): HtmlElement =
    div(
      cls := "admin__bar",
      renderTabs(sectionVar),
      button(
        typ := "button",
        cls := "btn btn--primary",
        child.text <-- sectionVar.signal.map(_.newLabel),
        onClick.compose(_.sample(sectionVar.signal)) --> openCreate
      )
    )

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
