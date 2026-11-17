package prototype.client.live

/** A stepper as the step-by-step panel drives it. S is the stepper's own type,
  * so each step returns the same kind of stepper; A is the type of the value it
  * shows.
  */
trait Stepper[+S <: Stepper[S, A], +A]:

  /** The stepper after adding one step (might be the same if it is not one of
    * its steps).
    */
  def addStep(step: TraceStep): S

  /** The stepper after one Next, or the same if there is no next step.
    * `skipIgnored`: whether the events the dashboard ignores are skipped or
    * shown, here and below.
    */
  def next(skipIgnored: Boolean): S

  /** Whether a Next would do something: a step left to show, a value to put on
    * screen, or a clean-up of what the last step left behind.
    */
  def hasNextStep(skipIgnored: Boolean): Boolean

  /** How many trace steps are left to show. */
  def stepsLeft(skipIgnored: Boolean): Int

  /** The value on screen. */
  def displayed: Option[A]
