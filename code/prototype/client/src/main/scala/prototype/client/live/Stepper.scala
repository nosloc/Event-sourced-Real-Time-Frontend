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

  /** The stepper after one Next. */
  def next(skipIgnored: Boolean): S

  def hasNextStep(skipIgnored: Boolean): Boolean

  def stepsLeft(skipIgnored: Boolean): Int

  /** The value on screen. */
  def displayed: Option[A]
