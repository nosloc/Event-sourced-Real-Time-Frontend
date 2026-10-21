package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.api.*
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.views.components.Icons
import prototype.client.views.components.PopoverPicker
import prototype.StatusTransitions.verifyStatusChange
import prototype.client.{ErrorMessages, InvalidInput}

/** The prestations of one appointment: a table (label, quantity, unit price,
  * total, status, actions), an add row and a back button. Shown in place of the
  * appointment details, inside the same `Modal` (see `AppointmentModal`).
  */
object PrestationsView:
  def apply(
      apptId: AppointmentId,
      prestationsVar: Var[List[Prestation]],
      showPrestationsVar: Var[Boolean]
  ): HtmlElement =
    val errorVar: Var[Option[String]] = Var(None)
    val showAddVar: Var[Boolean] = Var(false)
    val hasPrestationsS: Signal[Boolean] = prestationsVar.signal
      .combineWith(showAddVar)
      .map((ps, addvar) => ps.nonEmpty || addvar)
    div(
      cls := "prestations",
      child.maybe <-- errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      child <-- hasPrestationsS.signal.map {
        case false => div(cls := "prestations__empty", "No prestations yet")
        case true  =>
          div(
            cls := "prestations__scroll",
            table(
              cls := "prestations__table",
              thead(
                tr(
                  th("Label"),
                  th(cls := "num", "Qty"),
                  th(cls := "num", "Unit Price"),
                  th(cls := "num", "Total"),
                  th("Status"),
                  th()
                )
              ),
              tbody(
                children <-- prestationsVar.signal.split(
                  _.prestationId
                )((_, prestation, _) =>
                  renderPrestationRow(
                    prestation,
                    apptId,
                    prestationsVar,
                    errorVar
                  )
                ),
                renderAddPrestationRow(
                  showAddVar,
                  prestationsVar,
                  apptId,
                  errorVar
                )
              )
            )
          )
      },
      button(
        cls := "btn",
        "Add Prestation",
        onClick --> { _ => showAddVar.set(true) }
      ),
      button(
        cls := "btn",
        "Back",
        onClick --> { _ => showPrestationsVar.set(false) }
      )
    )
  end apply

  private def renderPrestationRow(
      prestation: Prestation,
      apptId: AppointmentId,
      prestationsVar: Var[List[Prestation]],
      errorVar: Var[Option[String]]
  ): HtmlElement =
    val totalPrice: Double = prestation.quantity * prestation.unitPrice
    // The last status confirmed by the server for this row (starts as the one
    // it was rendered with, and moves forward on each successful change) -
    // both the dropdown's choices and "did the picker change?" are computed
    // from this, not from the original `prestation`, so a second change in
    // the same window still offers the right choices.
    val currentStatusVar: Var[PrestationStatus] = Var(prestation.status)
    val selectedStatusVar: Var[PrestationStatus] = Var(prestation.status)
    val hasPendingChangeS: Signal[Boolean] = currentStatusVar.signal
      .combineWith(selectedStatusVar.signal)
      .map(_ != _)
    tr(
      td(prestation.label),
      td(cls := "num", prestation.quantity.toString),
      td(cls := "num", f"${prestation.unitPrice}%.2f"),
      td(cls := "num", f"$totalPrice%.2f"),
      td(
        cls := "prestations__status",
        // the cell stays a real table cell; this box does the flex layout
        div(
          cls := "prestations__status-box",
          PopoverPicker(
            id = s"status-${prestation.prestationId}",
            label = "",
            items = currentStatusVar.signal.map(current =>
              PrestationStatus.values
                .filter(p => verifyStatusChange(current, p) || p == current)
                .map { s =>
                  val itemLabel =
                    s.toString.replace("_", " ").toLowerCase().capitalize
                  PopoverPicker.Item(
                    name = itemLabel,
                    value = s,
                    buttonLabel = Some(itemLabel)
                  )
                }
                .toList
            ),
            selectedVar = selectedStatusVar,
            fallbackLabel = _.toString
          ),
          child.maybe <-- hasPendingChangeS.map(
            Option.when(_)(
              button(
                cls := "btn btn--small btn--icon btn--primary",
                aria.label := "Validate status change",
                title := "Validate status change",
                Icons.check,
                onClick
                  .compose(
                    _.sample(selectedStatusVar.signal)
                      .flatMapSwitch { newStatus =>
                        ApiCall
                          .fromIOCallSimple(
                            Api.appointments.updatePrestationStatus(
                              apptId,
                              prestation.prestationId,
                              newStatus
                            )
                          )
                          .stateS
                      }
                  ) --> {
                  case Api.Loading =>
                    errorVar.set(None)
                  case Api.Success(res) =>
                    errorVar.set(None)
                    prestationsVar.set(res.appointment.prestations)
                    res.appointment.prestations
                      .find(_.prestationId == prestation.prestationId)
                      .foreach(p => currentStatusVar.set(p.status))
                  case Api.Error(error) =>
                    errorVar.set(Some(ErrorMessages.errorToMessage(error)))
                }
              )
            )
          )
        )
      ),
      td(
        cls := "prestations__row-actions",
        button(
          cls := "btn btn--small btn--icon btn--danger",
          aria.label := "Delete",
          title := "Delete",
          Icons.trash,
          onClick.flatMap { _ =>
            ApiCall
              .fromIOCallSimple(
                Api.appointments
                  .deletePrestation(apptId, prestation.prestationId)
              )
              .stateS
          } --> {
            case Api.Loading =>
              errorVar.set(None)
            case Api.Success(_) =>
              errorVar.set(None)
              prestationsVar.update(
                _.filterNot(_.prestationId == prestation.prestationId)
              )
            case Api.Error(error) =>
              errorVar.set(Some(ErrorMessages.errorToMessage(error)))
          }
        )
      )
    )

  private def renderAddPrestationRow(
      showAddVar: Var[Boolean],
      prestationsVar: Var[List[Prestation]],
      appointmentId: AppointmentId,
      errorVar: Var[Option[String]]
  ): Mod[HtmlElement] =

    def field(v: Var[String], inputType: String, extra: Mod[HtmlElement]*) =
      input(
        typ := inputType,
        controlled(
          value <-- v.signal,
          onInput.mapToValue --> v.writer
        ),
        extra
      )

    child.maybe <-- showAddVar.signal.map {
      case true =>
        val labelVar: Var[String] = Var("")
        val quantityVar: Var[String] = Var("1")
        val unitPriceVar: Var[String] = Var("0.0")
        Some(
          tr(
            cls := "prestations__add",
            td(field(labelVar, "text", placeholder := "Label")),
            td(
              cls := "num",
              field(quantityVar, "number", minAttr := "1", stepAttr := "1")
            ),
            td(
              cls := "num",
              field(
                unitPriceVar,
                "number",
                minAttr := "0",
                stepAttr := "0.5"
              )
            ),
            td(cls := "num", "-"),
            td(
              cls := "prestations__status",
              div(
                cls := "prestations__status-box",
                // a new prestation always starts as "To bill": shown like the
                // other lines' status, but it cannot be changed
                PopoverPicker(
                  id = "status-new",
                  label = "",
                  items = Signal.fromValue(
                    List(
                      PopoverPicker.Item(
                        value = PrestationStatus.TO_BILL,
                        name = "To bill",
                        buttonLabel = Some("To bill")
                      )
                    )
                  ),
                  selectedVar = Var(PrestationStatus.TO_BILL),
                  locked = true
                )
              )
            ),
            td(
              cls := "prestations__row-actions",
              button(
                cls := "btn btn--primary btn--small btn--icon",
                aria.label := "Save",
                title := "Save",
                Icons.check,
                onClick
                  .compose(
                    _.sample(
                      labelVar.signal,
                      quantityVar.signal,
                      unitPriceVar.signal
                    )
                      .map((l, q, u) =>
                        (
                          l.trim,
                          q.toIntOption,
                          u.replace(",", ".").toFloatOption
                        )
                      )
                      .flatMapSwitch {
                        case (l, Some(q), Some(u)) =>
                          ApiCall
                            .fromIOCallSimple(
                              Api.appointments.createPrestation(
                                appointmentId,
                                l,
                                q,
                                u
                              )
                            )
                            .stateS
                        case (_, None, _) =>
                          Signal.fromValue(
                            Api.Error(
                              InvalidInput(
                                "The quantity must be a whole number."
                              )
                            )
                          )
                        case (_, _, None) =>
                          Signal.fromValue(
                            Api.Error(
                              InvalidInput("The unit price must be a number.")
                            )
                          )
                      }
                  ) -->
                  {
                    case Api.Loading =>
                      errorVar.set(None)
                    case Api.Success(res) =>
                      prestationsVar.set(res.appointment.prestations)
                      errorVar.set(None)
                      showAddVar.set(false)
                    case Api.Error(error) =>
                      errorVar.set(
                        Some(ErrorMessages.errorToMessage(error))
                      )
                  }
              ),
              button(
                cls := "btn btn--small btn--icon",
                aria.label := "Cancel",
                title := "Cancel",
                Icons.cross,
                onClick --> { _ => showAddVar.set(false) }
              )
            )
          )
        )
      case false => None
    }
