package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*
import prototype.api.EventView
import smithy4s.Blob
import smithy4s.json.Json
import EventSourcingModel.EventSourcingPage
import EventSourcingModel.EventSourcingPage.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.views.eventsourcing.EventSourcingModel.MaxEventsSize

/** The events of the event store, live: the newest ones as they are written,
  * and older pages on demand.
  */
object EventSourcingView:

  case class ShownHistoryPage(from: Long, to: Long, events: List[EventView]):
    def covers(skip: Long, size: Long): Boolean =
      from <= skip && skip + size <= to
    def slice(skip: Long, size: Long): List[EventView] =
      events.drop((skip - from).toInt).take(size.toInt)

  def apply(): HtmlElement =

    val receivedEvents: EventBus[String] = new EventBus[String]
    val opened: EventBus[Unit] = new EventBus[Unit]

    val showPageVar: Var[EventSourcingPage] = Var(LivePage)
    val liveEventsVar: Var[List[EventView]] = Var(List.empty)
    val historyPageVar: Var[Option[ShownHistoryPage]] = Var(None)

    val pageSizeVar: Var[Int] = Var(50)

    val askedHistoryS: Signal[Option[(Long, Int)]] =
      showPageVar.signal.combineWith(pageSizeVar.signal).map {
        case (LivePage, _)             => None
        case (HistoryPage(skip), size) => Some((skip, size))
      }

    // Loads the asked page, unless it is already held. Empty in Live
    def loadHistory(
        asked: Option[(Long, Int)]
    ): EventStream[ShownHistoryPage] =
      asked match
        case Some((skip, size))
            if !historyPageVar.now().exists(_.covers(skip, size)) =>
          ApiCall
            .fromIOCallSimple(Api.eventStore.listEvents(skip, size))
            .successS
            .changes
            .collectSome
            .map(page =>
              ShownHistoryPage(skip, skip + page.events.size, page.events)
            )
        case _ => EventStream.empty

    val loadedHistoryPage: EventStream[ShownHistoryPage] =
      askedHistoryS.changes.flatMapSwitch(loadHistory)

    val historyEventsS: Signal[List[EventView]] =
      historyPageVar.signal.combineWith(askedHistoryS).map {
        case (Some(page), Some((skip, size))) => page.slice(skip, size).reverse
        case _                                => List.empty
      }

    val liveEvent: EventStream[EventView] =
      receivedEvents.events
        .map(raw => Json.read[EventView](Blob(raw)).toOption)
        .collectSome

    val shownEventsS: Signal[List[EventView]] =
      showPageVar.signal
        .combineWith(liveEventsVar.signal, pageSizeVar.signal, historyEventsS)
        .map {
          case (LivePage, liveEvents, size, _)       => liveEvents.take(size)
          case (HistoryPage(_), _, _, historyEvents) => historyEvents
        }

    div(
      keepLiveEventsUpToDate(
        liveEvent,
        liveEventsVar,
        opened
      ),
      loadedHistoryPage --> historyPageVar.someWriter,
      EventSourcingModel.bind(receivedEvents, opened),
      cls := "event-sourcing",
      h1("Event store"),
      child <-- shownEventsS.map(_.toString)
    )
  end apply

  private def keepLiveEventsUpToDate(
      liveEvent: EventStream[EventView],
      liveEventsVar: Var[List[EventView]],
      opened: EventBus[Unit]
  ): Mod[HtmlElement] =
    Seq(
      opened.events.flatMapSwitch(_ =>
        ApiCall
          .fromIOCallSimple(Api.eventStore.getLatestEvents(MaxEventsSize))
          .successS
          .changes
          .collectSome
      ) --> (page =>
        // Merged: keeps the live events that arrived before the page
        liveEventsVar.update(current => mergeEvents(current, page.events))
      ),
      liveEvent --> { event =>
        liveEventsVar.update(current => mergeEvents(current, event))
      }
    )
  end keepLiveEventsUpToDate

  private def mergeEvents(
      oldEvents: List[EventView],
      newEvents: List[EventView]
  ): List[EventView] =
    val allEvents: Map[String, EventView] =
      (oldEvents ++ newEvents).map(e => e.eventId -> e).toMap
    allEvents.values.toList
      .sortBy(-_.position)
      .take(EventSourcingModel.MaxEventsSize)
  end mergeEvents

  private def mergeEvents(
      oldEvents: List[EventView],
      newEvent: EventView
  ): List[EventView] =
    mergeEvents(oldEvents, List(newEvent))
  end mergeEvents

end EventSourcingView
