package prototype.client.views.admin

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.views.components.Modal

/** The result of "Generate demo data", shown in a modal once the server has
  * answered: what was created, or that the data already existed.
  */
object DemoDataModal:

  enum Outcome:
    case Generated(output: GenerateDemoDataOutput)
    case AlreadyGenerated

  def apply(outcomeVar: Var[Option[Outcome]]): Mod[HtmlElement] =
    child.maybe <-- outcomeVar.signal.map(
      _.map(outcome =>
        Modal(
          title = outcome match
            case Outcome.Generated(_)     => "Demo data generated"
            case Outcome.AlreadyGenerated => "Demo data not generated",
          onClose = () => outcomeVar.set(None)
        )(
          outcome match
            case Outcome.Generated(output) => renderOutput(output)
            case Outcome.AlreadyGenerated  =>
              p(
                "Demo data already exists or is being generated, so nothing new was generated."
              )
          ,
          div(
            cls := "form__actions",
            button(
              typ := "button",
              cls := "btn btn--primary",
              "Close",
              onClick --> { _ => outcomeVar.set(None) }
            )
          )
        )
      )
    )

  private def renderOutput(output: GenerateDemoDataOutput): HtmlElement =
    val appointments = output.appointmentsByType.map(_.count).sum
    val prestations = output.prestationsByStatus.map(_.count).sum
    div(
      cls := "demo-data",
      p(
        cls := "demo-data__duration",
        f"Generated in ${output.durationMillis / 1000.0}%.1f s."
      ),
      dl(
        cls := "demo-data__counts",
        row("Companies", output.companies.toString),
        row(
          "Therapists",
          s"${output.therapists} (${output.therapistsWithAppointments} with appointments)"
        ),
        row("Patients", output.patients.toString),
        row("Appointments", appointments.toString),
        row("Prestations", prestations.toString)
      ),
      dl(
        cls := "demo-data__counts demo-data__counts--minor",
        aria.label := "Prestations by status",
        output.prestationsByStatus.map(c =>
          row(statusLabel(c.status), c.count.toString)
        )
      ),
      Option.when(output.skippedWrites > 0)(
        p(
          cls := "banner banner--error",
          s"${output.skippedWrites} writes were refused and skipped (see the server log)."
        )
      )
    )

  private def row(label: String, value: String): Seq[HtmlElement] =
    Seq(dt(label), dd(value))

  private def statusLabel(status: PrestationStatus): String =
    status match
      case PrestationStatus.TO_BILL   => "To bill"
      case PrestationStatus.BILLED    => "Billed"
      case PrestationStatus.PAID      => "Paid"
      case PrestationStatus.CANCELLED => "Cancelled"
end DemoDataModal
