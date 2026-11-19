package prototype.client.views.components

import prototype.client.live.{Diff, LiveData, SyncChecks, SyncStatus}

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Whether the live values of a view match the server: a dot and a word, and,
  * opened, one line per source with what its check found, then a button to a
  * modal that shows, for each id that differs, the browser's and the server's
  * versions side by side. Reading it runs the
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
    val detailsOpenVar: Var[Boolean] = Var(false)
    div(
      htmlTag("details")(
        cls := "sync",
        cls <-- statusS.map(status => s"sync--${modifier(status.level)}"),
        htmlTag("summary")(
          cls := "sync__summary",
          span(cls := "sync__dot", aria.hidden := true),
          span(text <-- statusS.map(status => label(status.level)))
        ),
        div(
          cls := "sync__panel",
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
          ),
          button(
            typ := "button",
            cls := "sync__more",
            "Details",
            // Closes the list, so it does not sit over the modal. The browser
            // owns whether a <details> is open (a click on its summary toggles
            // its `open` attribute, no Scala state involved), so the list is
            // closed by removing that attribute on the enclosing <details>.
            // A Var would have to follow the browser both ways (an `open` prop
            // bound to it, and `onToggle` to update it): two copies of one state
            onClick --> { event =>
              Option(
                event.target.asInstanceOf[dom.Element].closest("details")
              ).foreach(_.removeAttribute("open"))
              detailsOpenVar.set(true)
            }
          )
        )
      ),
      child.maybe <-- detailsOpenVar.signal.map(
        Option.when(_)(
          Modal("Compared with the server", () => detailsOpenVar.set(false))(
            cls := "modal__dialog--wide",
            div(
              cls := "sync-details",
              children <-- statusS.map(_.sources.map(renderSource))
            )
          )
        )
      )
    )
  end render

  // One source in the modal: its check, and both versions of what differs
  private def renderSource(source: SyncStatus.Source): HtmlElement =
    val diff: Option[Diff] = source.check match
      case SyncStatus.Check.Suspect(diff)     => Some(diff)
      case SyncStatus.Check.Diverged(diff, _) => Some(diff)
      case _                                  => None
    sectionTag(
      cls := s"sync-details__source sync__source--${modifier(source.check.level)}",
      h3(cls := "sync-details__name", source.name),
      p(cls := "sync-details__check", source.check.describe),
      diff.map(diff => div(diff.items.map(renderItem)))
    )

  // One id that differs: both versions side by side, as text
  private def renderItem(item: Diff.Item): HtmlElement =
    val kind = (item.local, item.server) match
      case (Some(_), None) => "only in the browser"
      case (None, Some(_)) => "only on the server"
      case _               => "different"
    def version(label: String, value: Option[String]): HtmlElement =
      div(
        cls := "sync-details__version",
        span(cls := "sync-details__side", label),
        pre(cls := "sync-details__value", value.getOrElse("–"))
      )
    div(
      cls := "sync-details__item",
      div(
        cls := "sync-details__id",
        code(item.id),
        span(cls := "sync-details__kind", kind)
      ),
      div(
        cls := "sync-details__versions",
        version("Browser", item.local),
        version("Server", item.server)
      )
    )

  private def label(level: SyncStatus.Level): String = level match
    case SyncStatus.Level.Synced   => "In sync"
    case SyncStatus.Level.Pending  => "Checking"
    case SyncStatus.Level.Failed   => "Check failed"
    case SyncStatus.Level.Diverged => "Out of sync"

  private def modifier(level: SyncStatus.Level): String =
    level.toString.toLowerCase
end SyncBadge
