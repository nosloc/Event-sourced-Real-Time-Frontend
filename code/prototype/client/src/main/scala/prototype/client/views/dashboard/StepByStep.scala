package prototype.client.views.dashboard

import prototype.client.dashboards.{ComputationLevel, DashboardSources}
import prototype.client.live.{LiveData, Tracer}

import com.raquo.laminar.api.L.*

/** The step-by-step mode of one dashboard on one computation level. A is the
  * type of the dashboard's value.
  */
trait StepByStep[A]:

  /** The real live data, which keeps running while the mode is on. */
  def live: LiveData[A]

  /** The value the stepper holds: it changes only at a display step, reached
    * with Next or Play, and starts again from [[live]] on Jump to latest.
    */
  def steppedData: LiveData[A]

  /** The panel: the controls, the current step and the graph of where each
    * event (and each reload or snapshot) is.
    */
  def panel: HtmlElement

object StepByStep:

  /** A dashboard's live data on both computation levels, with a stepper on
    * each.
    *
    * On either level the dashboard reads the real live data, or the stepper's
    * held one while the step-by-step mode is on. The real one keeps running
    * when the mode turns on: the switch starts the held one, which reads it,
    * before it lets go of the real one.
    *
    * @param tracer
    *   the tracer that records the steps
    * @param source
    *   the dashboard's name, the one its steps are recorded with
    * @param levelS
    *   the computation level, to switch between the two
    * @param server
    *   builds the live data reloaded from the server: a new one on each switch
    *   to the server level, since a stopped one is not read again
    * @param browser
    *   builds the live data computed in the browser, also on each switch
    * @param stepperObserver
    *   receives the stepper of the level shown, for the panel, and None once
    *   the dashboard leaves the page
    * @return
    *   the live data to show, and the modifier that hands the stepper to
    *   `stepperObserver`, to mount on the dashboard's element
    */
  def switchingLevels[A](
      tracer: Tracer,
      source: String,
      levelS: Signal[ComputationLevel],
      server: () => LiveData[A],
      browser: () => LiveData[A],
      stepperObserver: Observer[Option[StepByStep[?]]]
  ): (LiveData[A], Mod[HtmlElement]) =
    // One stepper per level change, shared by the dashboard and the panel
    val stepperS: Signal[StepByStep[A]] =
      levelS.distinct.map {
        case ComputationLevel.Server =>
          ServerStepByStep(tracer, source, server())
        case ComputationLevel.Browser =>
          BrowserStepByStep(tracer, source, DashboardSources.names, browser())
      }

    val data: LiveData[A] =
      LiveData.switching(stepperS)(stepper =>
        LiveData.switching(tracer.enabled.signal)(on =>
          if on then stepper.steppedData else stepper.live
        )
      )

    // Cleared once the dashboard leaves the page: the next one mounts after
    val handUp: Mod[HtmlElement] = Seq(
      stepperS.map(Some(_)) --> stepperObserver,
      onUnmountCallback(_ => stepperObserver.onNext(None))
    )

    (data, handUp)
