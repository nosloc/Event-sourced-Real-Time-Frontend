package prototype.client.views.faults

import prototype.api.{DelayMillis, Faults, ReadModel, ReadModelDelay}
import prototype.client.{Api, ErrorMessages}
import prototype.client.Api.ApiCall
import prototype.client.views.components.{FormRow, Modal}

import com.raquo.laminar.api.L.*
import smithy4s.http.RawErrorResponse

/** The delays the server adds on purpose, to make the live views' problems
  * happen on the running prototype: one after each snapshot, one per read
  * model. The server holds them in memory (a restart turns them off) and
  * answers 404 when it runs with faults off.
  */
object FaultsView:

  val MaxDelayMillis = 60000

  /** No fault at all: what "Turn all off" sends. */
  val none: Faults = Faults(DelayMillis(0), Nil)

  /** Whether any delay is on, for the "Faults on" pill. */
  def anyOn(faults: Faults): Boolean =
    faults.snapshotDelayMillis.value > 0 ||
      faults.readModelDelays.exists(_.delayMillis.value > 0)

  /** The form as typed: one text per delay, in milliseconds. */
  final case class FaultsForm(
      snapshot: String,
      readModels: Map[ReadModel, String],
      showErrors: Boolean = false
  ):
    def readModelText(readModel: ReadModel): String =
      readModels.getOrElse(readModel, "")

    def snapshotError: Option[String] = delayError(snapshot)
    def readModelError(readModel: ReadModel): Option[String] =
      delayError(readModelText(readModel))

    // Only the read models with a delay are sent
    def toFaults: Option[Faults] =
      val delays = ReadModel.values.map(readModel =>
        parseDelay(readModelText(readModel)).map(ReadModelDelay(readModel, _))
      )
      for
        snapshotDelay <- parseDelay(snapshot)
        readModelDelays <- Option.when(delays.forall(_.isDefined))(
          delays.flatten.filter(_.delayMillis.value > 0)
        )
      yield Faults(snapshotDelay, readModelDelays)

    def displayError(error: FaultsForm => Option[String]): Option[String] =
      error(this).filter(_ => showErrors)
  end FaultsForm

  object FaultsForm:
    def from(faults: Faults): FaultsForm =
      FaultsForm(
        faults.snapshotDelayMillis.value.toString,
        ReadModel.values
          .map(readModel => readModel -> delayOf(faults, readModel).toString)
          .toMap
      )

  // An empty field means no delay
  private def parseDelay(text: String): Option[DelayMillis] =
    val trimmed = text.trim
    (if trimmed.isEmpty then Some(0) else trimmed.toIntOption)
      .filter(delay => delay >= 0 && delay <= MaxDelayMillis)
      .map(DelayMillis(_))

  private def delayError(text: String): Option[String] =
    Option.when(parseDelay(text).isEmpty)(
      s"A whole number from 0 to $MaxDelayMillis"
    )

  // The server's rule: not listed is no delay, listed twice keeps the last
  private def delayOf(faults: Faults, readModel: ReadModel): Int =
    faults.readModelDelays
      .findLast(_.readModel == readModel)
      .fold(0)(_.delayMillis.value)

  def apply(): HtmlElement =
    val current: ApiCall[Faults] =
      ApiCall.fromIOCallSimple(Api.faults.getFaults())
    div(
      cls := "faults",
      div(
        cls := "faults__head",
        h1("Faults"),
        p(
          cls := "faults__intro",
          "Delays the server adds on purpose, to make the live views' " +
            "problems happen. Kept in memory: a server restart turns them " +
            "all off."
        )
      ),
      child <-- current.stateS.map {
        case Api.Loading         => p(cls := "faults__intro", "Loading…")
        case Api.Success(faults) => renderFaults(faults)
        case Api.Error(error: RawErrorResponse) if error.code == 404 =>
          div(
            cls := "banner banner--warning",
            "Fault injection is off on this server. Start it with ",
            code("FAULTS_ENABLED=true"),
            " to use this page."
          )
        case Api.Error(error) =>
          div(
            cls := "banner banner--error",
            ErrorMessages.errorToMessage(error)
          )
      }
    )
  end apply

  // The delays the server holds, one per line, with "Turn all off" and
  // "Edit" (the modal). Built once, from the faults read when the page opens
  private def renderFaults(initial: Faults): HtmlElement =
    // What the server holds, after its last answer
    val appliedVar: Var[Faults] = Var(initial)
    val editOpenVar: Var[Boolean] = Var(false)
    val turnOffBus: EventBus[Faults] = new EventBus
    val savingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)

    div(
      sending(turnOffBus.events, savingVar, errorVar, appliedVar.set),
      renderGroup(
        "Snapshots",
        "Added after the server read a list, before it sends it: the events " +
          "written meanwhile reach the browser during the wait.",
        List(
          renderDelay(
            "Every list",
            appliedVar.signal.map(_.snapshotDelayMillis.value)
          )
        )
      ),
      renderGroup(
        "Read models",
        "Added before a read model saves each event: the stream announces " +
          "it before the read model holds it. Slows the demo data down too, " +
          "and appointments also slows the appointment writes.",
        ReadModel.values.map(readModel =>
          renderDelay(
            label(readModel),
            appliedVar.signal.map(delayOf(_, readModel))
          )
        )
      ),
      child.maybe <-- errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      div(
        cls := "form__actions",
        button(
          typ := "button",
          cls := "btn",
          "Turn all off",
          disabled <-- appliedVar.signal
            .map(anyOn)
            .combineWith(savingVar.signal)
            .map((on, saving) => !on || saving),
          onClick.mapTo(none) --> turnOffBus.writer
        ),
        button(
          typ := "button",
          cls := "btn btn--primary",
          "Edit",
          disabled <-- savingVar.signal,
          onClick --> { _ => editOpenVar.set(true) }
        )
      ),
      child.maybe <-- editOpenVar.signal.map(
        Option.when(_)(
          renderEditModal(
            appliedVar.now(),
            applied => {
              appliedVar.set(applied)
              editOpenVar.set(false)
            },
            () => editOpenVar.set(false)
          )
        )
      )
    )
  end renderFaults

  // A titled box of delays, with what they do
  private def renderGroup(
      title: String,
      hint: String,
      delays: List[HtmlElement]
  ): HtmlElement =
    sectionTag(
      cls := "faults__group",
      h2(cls := "faults__title", title),
      p(cls := "faults__hint", hint),
      ul(cls := "faults__delays", delays)
    )

  // One delay as the server holds it: "Off", or its value in milliseconds
  private def renderDelay(name: String, delayS: Signal[Int]): HtmlElement =
    li(
      cls := "faults__delay",
      span(cls := "faults__name", name),
      span(
        cls := "faults__value",
        cls("faults__value--on") <-- delayS.map(_ > 0),
        text <-- delayS.map(delay => if delay > 0 then s"$delay ms" else "Off")
      )
    )

  // The form, filled with what the server holds when it opens
  private def renderEditModal(
      initial: Faults,
      onApplied: Faults => Unit,
      onClose: () => Unit
  ): HtmlElement =
    val formVar: Var[FaultsForm] = Var(FaultsForm.from(initial))
    val applyBus: EventBus[Faults] = new EventBus
    val savingVar: Var[Boolean] = Var(false)
    val errorVar: Var[Option[String]] = Var(None)

    Modal("Edit the faults", onClose)(
      form(
        sending(applyBus.events, savingVar, errorVar, onApplied),
        onSubmit.preventDefault --> { _ =>
          formVar.update(_.copy(showErrors = true))
        },
        onSubmit.preventDefault.compose(
          _.sample(formVar.signal)
            .map(_.toFaults)
            .collect { case Some(faults) => faults }
        ) --> applyBus.writer,
        p(cls := "faults__hint", "In milliseconds; 0 or empty turns it off."),
        h3(cls := "faults__subtitle", "Snapshots"),
        FormRow.input(
          "Every list",
          "number",
          formVar.zoomLazy(_.snapshot)((form, text) =>
            form.copy(snapshot = text)
          ),
          formVar.signal.map(_.displayError(_.snapshotError))
        ),
        h3(cls := "faults__subtitle", "Read models"),
        ReadModel.values.map(readModel =>
          FormRow.input(
            label(readModel),
            "number",
            formVar.zoomLazy(_.readModelText(readModel))((form, text) =>
              form.copy(readModels = form.readModels.updated(readModel, text))
            ),
            formVar.signal.map(_.displayError(_.readModelError(readModel)))
          )
        ),
        child.maybe <-- errorVar.signal.map(
          _.map(msg => div(cls := "banner banner--error", msg))
        ),
        div(
          cls := "form__actions",
          button(
            typ := "button", // "submit" is the default in HTML
            cls := "btn",
            "Cancel",
            onClick --> { _ => onClose() }
          ),
          button(
            typ := "submit",
            cls := "btn btn--primary",
            "Apply",
            disabled <-- savingVar.signal
          )
        )
      )
    )
  end renderEditModal

  // Sends each faults to the server (a new one cancels the wait for the
  // previous answer) and reports: busy while it runs, the error if it fails,
  // what the server holds if it succeeds (to the page and to `FaultsStatus`)
  private def sending(
      faults: EventStream[Faults],
      savingVar: Var[Boolean],
      errorVar: Var[Option[String]],
      onApplied: Faults => Unit
  ): Binder.Base =
    faults.flatMapSwitch(faults =>
      ApiCall.fromIOCallSimple(Api.faults.setFaults(faults)).stateS
    ) --> {
      case Api.Loading =>
        savingVar.set(true)
        errorVar.set(None)
      case Api.Success(applied) =>
        savingVar.set(false)
        onApplied(applied)
        // For the "Faults on" pill
        FaultsStatus.emit(applied)
      case Api.Error(error) =>
        savingVar.set(false)
        errorVar.set(Some(ErrorMessages.errorToMessage(error)))
    }

  private def label(readModel: ReadModel): String = readModel match
    case ReadModel.COMPANIES    => "Companies"
    case ReadModel.THERAPISTS   => "Therapists"
    case ReadModel.PATIENTS     => "Patients"
    case ReadModel.APPOINTMENTS => "Appointments"
end FaultsView
