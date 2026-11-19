package prototype.client.views.components

import prototype.client.live.{LiveData, SyncChecks, SyncStatus}

import com.raquo.laminar.api.L.*

/** Whether the live values of a view match the server: a dot and a word, and,
  * opened, one line per source with what its check found. Reading it runs the
  * checks (and the values, if nothing else reads them), so it shows only while
  * the global switch (`SyncChecks`) is on: off, it is an empty element and no
  * check runs.
  */
object SyncBadge:

  def apply(values: LiveData[?]*): HtmlElement =
    div(
      cls := "sync-slot",
      child.maybe <-- SyncChecks.enabledS.distinct.map(
        Option.when(_)(badge(values))
      )
    )

  // Built each time the switch turns on. The checks' state lives in the
  // sources, so it resumes where it stopped: a read cut off by the switch is
  // given up after `SyncCheckModel.MaxTicksInFlight` checks of the stream
  private def badge(values: Seq[LiveData[?]]): HtmlElement =
    val statusS: Signal[SyncStatus] =
      Signal
        .combineSeq(values.map(_.syncS))
        .map(_.foldLeft(SyncStatus.nothingToCheck)(_ ++ _))
    render(statusS)

  private def render(statusS: Signal[SyncStatus]): HtmlElement =
    htmlTag("details")(
      cls := "sync",
      cls <-- statusS.map(status => s"sync--${modifier(status.level)}"),
      htmlTag("summary")(
        cls := "sync__summary",
        span(cls := "sync__dot", aria.hidden := true),
        span(text <-- statusS.map(status => label(status.level)))
      ),
      ul(
        cls := "sync__sources",
        children <-- statusS.map(
          _.sources.map(source =>
            li(
              cls := s"sync__source sync__source--${modifier(source.check.level)}",
              span(cls := "sync__name", source.name),
              span(cls := "sync__check", source.check.describe)
            )
          )
        )
      )
    )
  end render

  private def label(level: SyncStatus.Level): String = level match
    case SyncStatus.Level.Synced   => "In sync"
    case SyncStatus.Level.Pending  => "Checking"
    case SyncStatus.Level.Failed   => "Check failed"
    case SyncStatus.Level.Diverged => "Out of sync"

  private def modifier(level: SyncStatus.Level): String =
    level.toString.toLowerCase
end SyncBadge
