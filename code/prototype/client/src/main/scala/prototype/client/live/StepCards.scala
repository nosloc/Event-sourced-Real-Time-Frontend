package prototype.client.live

/** What the step-by-step panel shows when an event, a reload or a snapshot is
  * clicked on its graph. The steppers keep one while the object is on the
  * graph, and drop it when it leaves.
  */
object StepCards:

  /** An event: its type and payload, and the steps shown so far. */
  final case class EventCard(
      eventType: String,
      payload: String,
      steps: Vector[TraceStep]
  )

  /** A server reload: what was asked, when it left, and when it was answered
    * or failed.
    */
  final case class LoadCard(
      request: String,
      launchedAt: Double,
      answeredAt: Option[Double],
      failed: Boolean
  )

  /** A snapshot: what was asked, when, and how many items came back. */
  final case class SnapshotCard(
      source: String,
      request: String,
      requestedAt: Option[Double],
      receivedAt: Option[Double],
      size: Option[Int]
  )
