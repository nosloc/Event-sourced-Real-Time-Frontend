package prototype.client

import com.raquo.laminar.api.L.{*, given}
import prototype.api.*

object TherapistView:

  def render: HtmlElement =
    val therapistsVar: Var[Option[List[Therapist]]] = Var(None)
    val errorVar: Var[Option[String]] = Var(None)
    div(
      h1("Therapists"),
      child.maybe <-- errorVar.signal.map(
        _.map(error => div(color.red, s"Error: $error"))
      ),
      child <-- therapistsVar.signal.map {
        case Some(therapists) => renderTherapists(therapists)
        case None             => loadButton(therapistsVar, errorVar)
      }
    )
  end render

  private def renderTherapists(therapists: List[Therapist]): Element =
    div(
      table(
        thead(
          tr(
            th("First Name"),
            th("Last Name"),
            th("Trigram"),
            th("Company name"),
            th("Company address")
          )
        ),
        tbody(
          therapists.map { therapist =>
            tr(
              td(therapist.firstName),
              td(therapist.lastName),
              td(therapist.trigram),
              td(therapist.company.name),
              td(therapist.company.address)
            )
          }
        )
      )
    )
  end renderTherapists

  def loadButton(
      therapistsVar: Var[Option[List[Therapist]]],
      errorVar: Var[Option[String]]
  ): Element =
    button(
      "Load Therapists",
      onClick.flatMap(_ => Api.stream(Api.therapists.getTherapists())) -->
        {
          case Right(output) =>
            therapistsVar.set(Some(output.therapists))
            errorVar.set(None)
          case Left(error) =>
            errorVar.set(Some(error.getMessage))
        }
    )
