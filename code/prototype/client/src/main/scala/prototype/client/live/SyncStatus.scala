package prototype.client.live

import prototype.client.live.LiveFeed.StreamStatus

/** Whether a live value matches the server, per source it comes from: a value
  * built from several sources (`map`, `combineWith`) carries the checks of all
  * of them, each under its name, so a view can tell which one is wrong.
  */
final case class SyncStatus(sources: List[SyncStatus.Source]):

  /** The worst level of its sources; `Synced` when there is nothing to check.
    */
  def level: SyncStatus.Level =
    sources
      .map(_.check.level)
      .maxByOption(_.ordinal)
      .getOrElse(SyncStatus.Level.Synced)

  /** Both statuses' sources. A source read twice (the company's therapists, in
    * the appointments' filter and in the dashboard) is kept once.
    */
  def ++(that: SyncStatus): SyncStatus =
    SyncStatus((sources ++ that.sources).distinctBy(_.name))

object SyncStatus:

  /** The status of a value with nothing to check (a plain signal). */
  val nothingToCheck: SyncStatus = SyncStatus(Nil)

  def of(name: String, check: Check): SyncStatus =
    SyncStatus(List(Source(name, check)))

  final case class Source(name: String, check: Check)

  /** From best to worst: a combined status shows its worst source. */
  enum Level:
    case Synced
    case Pending
    case Failed
    case Diverged

  /** Where the check of one source stands. */
  enum Check:
    // Nothing compared yet
    case NotChecked
    // The stream is not up to date: a comparison would mean nothing
    case WaitingForStream(stream: StreamStatus)
    // The source is loading (first load, new key): nothing to compare yet
    case Loading
    // The last comparison found the same value as the server
    case Synced
    // One comparison differed: maybe a read model still catching up, checked
    // again before it counts
    case Suspect(diff: Diff)
    // Differed `checks` times in a row: a real difference
    case Diverged(diff: Diff, checks: Int)
    // The server's value could not be read, `failures` times in a row
    case Failed(error: Throwable, failures: Int)

    def level: Level = this match
      case Synced                                   => Level.Synced
      case NotChecked | WaitingForStream(_) | Loading | Suspect(_) =>
        Level.Pending
      case Failed(_, _)   => Level.Failed
      case Diverged(_, _) => Level.Diverged

    /** In words, for the badge. */
    def describe: String = this match
      case NotChecked              => "not checked yet"
      case WaitingForStream(stream) =>
        stream match
          case StreamStatus.Closed           => "waiting: the stream is closed"
          case StreamStatus.Behind(head, at) =>
            s"waiting: the stream is ${head - at} events behind"
          case StreamStatus.Ahead(_, _) =>
            "waiting: the stream is ahead of the store"
          case StreamStatus.CheckFailed(_, _, _) =>
            "waiting: the stream's check failed"
          case StreamStatus.NotChecked | StreamStatus.UpToDate(_) =>
            "waiting for the stream's check"
      case Loading           => "loading"
      case Synced            => "matches the server"
      case Suspect(diff)     => s"differs, checking again (${diff.summary})"
      case Diverged(diff, n) => s"differs, $n checks in a row (${diff.summary})"
      case Failed(error, n)  =>
        s"the server could not be read ($n in a row): ${error.getMessage}"

  end Check
end SyncStatus
