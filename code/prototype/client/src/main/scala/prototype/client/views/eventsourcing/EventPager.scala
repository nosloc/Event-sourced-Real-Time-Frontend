package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*
import prototype.client.views.components.{Icons, PopoverPicker}
import prototype.client.views.eventsourcing.EventSourcingModel.*
import prototype.client.views.eventsourcing.EventSourcingModel.EventSourcingPage.LivePage

/** Rows per page, the range shown, then newest / newer / older / oldest. The
  * tab owns the state: the pager only shows it and calls back.
  */
object EventPager:

  final case class Moves(
      goLive: () => Unit,
      goNewer: () => Unit,
      goOlder: () => Unit,
      goOldest: () => Unit
  )

  def apply(
      stateS: Signal[PagerState],
      sizeChoiceVar: Var[Int],
      moves: Moves
  ): HtmlElement =
    val canNewerS = stateS.map(_.page != LivePage)
    val canOlderS =
      stateS.map(s => canOlder(s.page, s.total, s.size, s.anchor))
    div(
      cls := "pager",
      PopoverPicker(
        id = "event-page-size",
        label = "Rows per page",
        items = Val(PageSizes.map(n => PopoverPicker.Item(n, n.toString))),
        selectedVar = sizeChoiceVar
      ),
      span(
        cls := "pager__range",
        child.text <-- stateS.map(s =>
          rangeText(s.page, s.total, s.size, s.anchor)
        )
      ),
      child.maybe <-- stateS.map(s =>
        newEvents(s.page, s.total, s.anchor)
          .map(count => span(cls := "pager__new", s"+$count new"))
      ),
      div(
        cls := "pager__buttons",
        pagerButton(
          "Newest (live)",
          Icons.chevronFirst,
          canNewerS,
          moves.goLive
        ),
        pagerButton("Newer", Icons.chevronLeft, canNewerS, moves.goNewer),
        pagerButton("Older", Icons.chevronRight, canOlderS, moves.goOlder),
        pagerButton("Oldest", Icons.chevronLast, canOlderS, moves.goOldest)
      )
    )
  end apply

  private def pagerButton(
      name: String,
      icon: SvgElement,
      enabledS: Signal[Boolean],
      onPress: () => Unit
  ): HtmlElement =
    button(
      typ := "button",
      cls := "pager__button",
      aria.label := name,
      title := name,
      icon,
      disabled <-- enabledS.map(!_),
      onClick --> { _ => onPress() }
    )
end EventPager
