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
      apptS: Signal[Appointment],
      showPrestationsVar: Var[Boolean]
  ): HtmlElement =
    val errorVar: Var[Option[String]] = Var(None)
    // Shown where the error is: someone else's change made the user's work
    // outdated
    val warningVar: Var[Option[String]] = Var(None)
    val showAddVar: Var[Boolean] = Var(false)
    // The status each row has picked and not seen confirmed yet, to warn when
    // someone else deletes that prestation
    val choicesVar: Var[Map[PrestationId, PrestationStatus]] = Var(Map.empty)
    // The labels of the prestations last seen, to name one once deleted
    val knownVar: Var[Map[PrestationId, String]] = Var(Map.empty)

    val prestationsS: Signal[List[Prestation]] = apptS.map(_.prestations)
    val hasPrestationsS: Signal[Boolean] = prestationsS
      .combineWith(showAddVar)
      .map((ps, addvar) => ps.nonEmpty || addvar)
      .distinct
    div(
      cls := "prestations",
      // A prestation that disappears while its row had a choice pending was
      // deleted by someone else (deleting it here drops the choice first)
      prestationsS.withCurrentValueOf(knownVar.signal, choicesVar.signal) --> {
        (prestations, known, choices) =>
          val current = prestations.map(p => p.prestationId -> p.label).toMap
          val deleted = known.keySet -- current.keySet
          deleted
            .filter(choices.contains)
            .headOption
            .foreach(id =>
              warningVar.set(
                Some(s"Someone deleted the prestation \"${known(id)}\".")
              )
            )
          knownVar.set(current)
          choicesVar.update(_ -- deleted)
      },
      child.maybe <-- errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      child.maybe <-- warningVar.signal.map(
        _.map(msg => div(cls := "banner banner--warning", msg))
      ),
      // A warning goes away after 3 seconds; a new one restarts the delay
      warningVar.signal.changes
        .collect { case Some(_) => () }
        .flatMapSwitch(_ => EventStream.delay(3000))
        --> { _ => warningVar.set(None) },
      child <-- hasPrestationsS.map { hasPrestations =>
        hasPrestations match
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
                // One tbody per prestation: its row and, under it, its
                // warning
                children <-- prestationsS.split(
                  _.prestationId
                )((_, initial, prestationS) =>
                  renderPrestationRow(
                    prestationS,
                    initial,
                    apptId,
                    errorVar,
                    choicesVar
                  )
                ),
                tbody(
                  renderAddPrestationRow(
                    showAddVar,
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
      prestationS: Signal[Prestation],
      initial: Prestation,
      apptId: AppointmentId,
      errorVar: Var[Option[String]],
      choicesVar: Var[Map[PrestationId, PrestationStatus]]
  ): HtmlElement =
    // Label, quantity and price never change after creation; the status is
    // live
    val totalPrice: Double = initial.quantity * initial.unitPrice
    val liveStatusS: Signal[PrestationStatus] =
      prestationS.map(_.status).distinct
    // Where the status change of this row stands. Idle: the picker shows the
    // live status
    val pendingVar: Var[Pending] = Var(Pending.Idle)
    val staleS: Signal[Boolean] = pendingVar.signal.map(_.isStale)
    // No new pick or validation while a choice is stale or on its way
    val blockedS: Signal[Boolean] =
      pendingVar.signal.map(pending => pending.isStale || pending.isSent)
    val pickBus: EventBus[PrestationStatus] = new EventBus

    val shownStatusS: Signal[PrestationStatus] = pendingVar.signal
      .combineWith(liveStatusS)
      .map((pending, live) => pending.choice.getOrElse(live))

    // Shown under the row until the user cancels the stale choice
    val warningS: Signal[Option[String]] = pendingVar.signal
      .map {
        case (Pending.Stale(_)) =>
          Some(
            s"Someone changed the status concurrently. Cancel your change to see the current status."
          )
        case _ => None
      }

    val row = tr(
      pickBus.events.withCurrentValueOf(liveStatusS) --> { (choice, live) =>
        pendingVar.set(Pending.picked(choice, live))
      },
      liveStatusS.changes --> { live =>
        pendingVar.update(_.afterLiveChange(live))
      },
      // The view needs the pending choice to warn if the prestation is deleted
      pendingVar.signal --> { pending =>
        choicesVar.update(choices =>
          pending.choice.fold(choices - initial.prestationId)(choice =>
            choices.updated(initial.prestationId, choice)
          )
        )
      },
      td(initial.label),
      td(cls := "num", initial.quantity.toString),
      td(cls := "num", f"${initial.unitPrice}%.2f"),
      td(cls := "num", f"$totalPrice%.2f"),
      td(
        cls := "prestations__status",
        // the cell stays a real table cell; this box does the flex layout
        div(
          cls := "prestations__status-box",
          PopoverPicker.controlled(
            id = s"status-${initial.prestationId}",
            label = "",
            items = liveStatusS.map(statusItems),
            selectedS = shownStatusS,
            onSelect = pickBus.writer,
            fallbackLabel = statusLabel,
            lockedS = blockedS
          ),
          child.maybe <-- pendingVar.signal
            .map(_ != Pending.Idle)
            .distinct
            .map(
              Option.when(_)(
                renderValidateButton(
                  pendingVar,
                  blockedS,
                  apptId,
                  initial.prestationId,
                  errorVar
                )
              )
            ),
          child.maybe <-- staleS.map(
            Option.when(_)(
              button(
                cls := "btn btn--small btn--icon",
                aria.label := "Cancel the status change",
                title := "Cancel the status change",
                Icons.cross,
                onClick --> { _ => pendingVar.set(Pending.Idle) }
              )
            )
          )
        )
      ),
      td(
        cls := "prestations__row-actions",
        // Deleting it here drops the pending choice, so the view does not
        // take it for someone else's deletion
        renderDeleteButton(
          apptId,
          initial.prestationId,
          errorVar,
          onDelete = () => pendingVar.set(Pending.Idle)
        )
      )
    )

    tbody(
      cls := "prestations__item",
      row,
      child.maybe <-- warningS.map(
        _.map(msg =>
          tr(
            cls := "prestations__warning",
            td(colSpan := 6, msg)
          )
        )
      )
    )
  end renderPrestationRow

  // The statuses the picker offers: the current one and those it can move to
  private def statusItems(
      current: PrestationStatus
  ): List[PopoverPicker.Item[PrestationStatus]] =
    PrestationStatus.values
      .filter(p => verifyStatusChange(current, p) || p == current)
      .map { s =>
        val itemLabel = statusLabel(s)
        PopoverPicker.Item(
          name = itemLabel,
          value = s,
          buttonLabel = Some(itemLabel)
        )
      }
      .toList

  // Sends the picked status. The new status comes back through the event
  // stream, which moves the row out of Sent (see `Pending.afterLiveChange`)
  private def renderValidateButton(
      pendingVar: Var[Pending],
      blockedS: Signal[Boolean],
      apptId: AppointmentId,
      prestationId: PrestationId,
      errorVar: Var[Option[String]]
  ): HtmlElement =
    // a disabled button does not show its tooltip in every browser, so the
    // wrapper carries it. Why it is disabled is told in the warning banner
    span(
      cls := "prestations__validate",
      title := "Validate status change",
      button(
        cls := "btn btn--small btn--icon btn--primary",
        aria.label := "Validate status change",
        disabled <-- blockedS,
        Icons.check,
        onClick
          .compose(
            _.sample(pendingVar.signal)
              .collect { case Pending.Picked(newStatus) => newStatus }
              .flatMapSwitch { newStatus =>
                ApiCall
                  .fromIOCallSimple(
                    Api.appointments.updatePrestationStatus(
                      apptId,
                      prestationId,
                      newStatus
                    )
                  )
                  .stateS
                  .map(state => (newStatus, state))
              }
          ) --> {
          case (newStatus, Api.Loading) =>
            errorVar.set(None)
            pendingVar.set(Pending.Sent(newStatus))
          case (_, Api.Success(_)) =>
            errorVar.set(None)
          case (_, Api.Error(error)) =>
            errorVar.set(Some(ErrorMessages.errorToMessage(error)))
            pendingVar.update(_.afterError)
        }
      )
    )
  end renderValidateButton

  private def renderDeleteButton(
      apptId: AppointmentId,
      prestationId: PrestationId,
      errorVar: Var[Option[String]],
      onDelete: () => Unit
  ): HtmlElement =
    button(
      cls := "btn btn--small btn--icon btn--danger",
      aria.label := "Delete",
      title := "Delete",
      Icons.trash,
      onClick.flatMap { _ =>
        onDelete()
        ApiCall
          .fromIOCallSimple(
            Api.appointments.deletePrestation(apptId, prestationId)
          )
          .stateS
      } --> {
        case Api.Loading =>
          errorVar.set(None)
        case Api.Success(_) =>
          errorVar.set(None)
        case Api.Error(error) =>
          errorVar.set(Some(ErrorMessages.errorToMessage(error)))
      }
    )
  end renderDeleteButton

  // The status change of one row, from the user's pick to the server's event.
  // The transitions are pure, the row only feeds them
  private enum Pending:
    case Idle
    // Chosen, not validated yet
    case Picked(status: PrestationStatus)
    // Validated, waiting for its event to come back through the stream
    case Sent(status: PrestationStatus)
    // The live status changed under the choice: only cancel clears it
    case Stale(status: PrestationStatus)

    // The status the picker shows instead of the live one
    def choice: Option[PrestationStatus] = this match
      case Idle           => None
      case Picked(status) => Some(status)
      case Sent(status)   => Some(status)
      case Stale(status)  => Some(status)

    def isStale: Boolean = this match
      case Stale(_) => true
      case _        => false

    def isSent: Boolean = this match
      case Sent(_) => true
      case _       => false

    // Only a sent choice can be confirmed by the live status. Any other live
    // change, even to the same status, was made by someone else: the choice
    // was made against an outdated status
    def afterLiveChange(live: PrestationStatus): Pending = this match
      case Sent(status) if status == live => Idle
      case Sent(status)                   => Stale(status)
      case Picked(status)                 => Stale(status)
      case other                          => other

    // Back to picked, to retry or change the choice, unless someone else's
    // change made it stale meanwhile
    def afterError: Pending = this match
      case Sent(status) => Picked(status)
      case other        => other

  private object Pending:
    // Picking the live status itself leaves nothing pending
    def picked(choice: PrestationStatus, live: PrestationStatus): Pending =
      if choice == live then Idle else Picked(choice)

  private def statusLabel(status: PrestationStatus): String =
    status.toString.replace("_", " ").toLowerCase().capitalize

  private def renderAddPrestationRow(
      showAddVar: Var[Boolean],
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
                  lockedS = Signal.fromValue(true)
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
                    case Api.Success(_) =>
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
