package prototype.client.views.eventsourcing

import com.raquo.laminar.api.L.*
import prototype.api.EventView
import EventSourcingModel.*
import EventSourcingModel.EventSourcingPage.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.ErrorMessages
import prototype.client.live.LiveFeed

/** The events of the event store, live: the newest ones as they are written,
  * and older pages on demand. Holds the state; the rules are in
  * `EventSourcingModel`.
  */
object EventSourcingView:

  def apply(): HtmlElement =

    val receivedEvents: EventBus[String] = new EventBus[String]
    val opened: EventBus[Unit] = new EventBus[Unit]

    val showPageVar: Var[EventSourcingPage] = Var(LivePage)
    val liveEventsVar: Var[List[EventView]] = Var(List.empty)
    val historyPageVar: Var[Option[ShownHistoryPage]] = Var(None)

    val pageSizeVar: Var[Int] = Var(50)
    // What the size picker writes; the change goes through setPageSize
    val sizeChoiceVar: Var[Int] = Var(pageSizeVar.now())
    // Events in the store, counted from the latest page and the live events
    val totalEventsVar: Var[Long] = Var(0L)
    // The live event to highlight, for a short time after it arrived
    val arrivedVar: Var[Option[String]] = Var(None)
    // The number of events when the live window was left: the pages are
    // counted from it, so they do not move while new events arrive
    val anchorVar: Var[Long] = Var(0L)

    val errorVar: Var[Option[String]] = Var(None)
    val historyLoadingVar: Var[Boolean] = Var(false)
    val selectedVar: Var[Option[EventView]] = Var(None)

    val askedHistoryS: Signal[Option[(Long, Int)]] =
      showPageVar.signal
        .combineWith(pageSizeVar.signal, anchorVar.signal)
        .map {
          case (LivePage, _, _)                    => None
          case (HistoryPage(number), size, anchor) =>
            Some(pageBounds(number, size, anchor))
        }

    // Loads the asked page, unless it is already held. None: nothing loads
    def loadHistory(
        asked: Option[(Long, Int)]
    ): EventStream[Option[Api.ApiState[ShownHistoryPage]]] =
      asked match
        case Some((skip, size))
            if !historyPageVar.now().exists(_.covers(skip, size)) =>
          val call = ApiCall.fromIOCall(
            Api.eventStore.listEvents(skip, size),
            page => ShownHistoryPage(skip, skip + page.events.size, page.events)
          )
          EventStream
            .merge(EventStream.fromValue(Api.Loading), call.stateS.changes)
            .map(Some(_))
        case _ => EventStream.fromValue(None)

    val loadedHistoryPage: EventStream[Option[Api.ApiState[ShownHistoryPage]]] =
      askedHistoryS.changes.flatMapSwitch(loadHistory)

    val historyEventsS: Signal[List[EventView]] =
      historyPageVar.signal.combineWith(askedHistoryS).map {
        case (Some(page), Some((skip, size))) => page.slice(skip, size).reverse
        case _                                => List.empty
      }

    val liveEvent: EventStream[EventView] =
      receivedEvents.events.map(decode).collectSome

    // While a page loads, nothing rather than the previous page
    val shownEventsS: Signal[List[EventView]] =
      showPageVar.signal
        .combineWith(
          liveEventsVar.signal,
          pageSizeVar.signal,
          historyEventsS,
          historyLoadingVar.signal
        )
        .map {
          case (LivePage, liveEvents, size, _, _) => liveEvents.take(size)
          case (HistoryPage(_), _, _, _, true)    => List.empty
          case (HistoryPage(_), _, _, events, _)  => events
        }

    def move(to: Move): Unit =
      to.anchor match
        case Some(anchor) =>
          Var.set(anchorVar -> anchor, showPageVar -> to.page)
        case None => showPageVar.set(to.page)

    val moves = EventPager.Moves(
      goLive = () => showPageVar.set(LivePage),
      goNewer = () => showPageVar.update(newer),
      goOlder = () => move(older(showPageVar.now(), totalEventsVar.now())),
      goOldest = () =>
        move(
          oldest(
            showPageVar.now(),
            pageSizeVar.now(),
            totalEventsVar.now(),
            anchorVar.now()
          )
        )
    )

    // Size and page together, so the page loads once
    def setPageSize(size: Int): Unit =
      Var.set(
        pageSizeVar -> size,
        showPageVar -> resized(showPageVar.now(), pageSizeVar.now(), size)
      )

    val pagerStateS: Signal[PagerState] =
      showPageVar.signal
        .combineWith(
          totalEventsVar.signal,
          pageSizeVar.signal,
          anchorVar.signal
        )
        .map(PagerState.apply)

    val emptyTextS: Signal[String] =
      showPageVar.signal.combineWith(historyLoadingVar.signal).map {
        case (HistoryPage(_), true) => "Loading…"
        case _                      => "No events yet."
      }

    div(
      cls := "event-sourcing",
      keepLiveEventsUpToDate(
        liveEvent,
        liveEventsVar,
        totalEventsVar,
        arrivedVar,
        errorVar,
        opened
      ),
      loadedHistoryPage --> {
        case Some(Api.Loading) =>
          historyLoadingVar.set(true)
        case Some(Api.Success(page)) =>
          historyLoadingVar.set(false)
          errorVar.set(None)
          historyPageVar.set(Some(page))
        case Some(Api.Error(error)) =>
          historyLoadingVar.set(false)
          errorVar.set(Some(ErrorMessages.errorToMessage(error)))
        case None =>
          historyLoadingVar.set(false)
      },
      LiveFeed.bind(receivedEvents, opened),
      sizeChoiceVar.signal.changes --> { size => setPageSize(size) },
      div(
        cls := "event-sourcing__head",
        div(
          cls := "event-sourcing__title",
          h1("Event store"),
          child <-- pagerStateS
            .map(s => pageLabel(s.page, s.size, s.anchor))
            .map {
              case None =>
                span(
                  cls := "event-sourcing__marker event-sourcing__marker--live",
                  "Live"
                )
              case Some(label) => span(cls := "event-sourcing__marker", label)
            }
        ),
        EventPager(pagerStateS, sizeChoiceVar, moves)
      ),
      child.maybe <-- errorVar.signal.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      EventTable(
        shownEventsS,
        arrivedVar.signal,
        emptyTextS,
        event => selectedVar.set(Some(event))
      ),
      EventDetailsModal(selectedVar)
    )
  end apply

  private def keepLiveEventsUpToDate(
      liveEvent: EventStream[EventView],
      liveEventsVar: Var[List[EventView]],
      totalEventsVar: Var[Long],
      arrivedVar: Var[Option[String]],
      errorVar: Var[Option[String]],
      opened: EventBus[Unit]
  ): Mod[HtmlElement] =
    Seq(
      // once the stream is open (also after a reconnect), for an event written in between
      opened.events
        .flatMapSwitch(_ =>
          ApiCall
            .fromIOCallSimple(Api.eventStore.getLatestEvents(MaxEventsSize))
            .stateS
            .changes
        ) --> {
        case Api.Success(page) =>
          errorVar.set(None)
          totalEventsVar.set(totalAfterLatest(page, liveEventsVar.now()))
          // Merged: keeps the live events that arrived before the page
          liveEventsVar.update(current => mergeEvents(current, page.events))
        case Api.Error(error) =>
          errorVar.set(Some(ErrorMessages.errorToMessage(error)))
        case Api.Loading => ()
      },
      liveEvent --> { event =>
        if isNewEvent(liveEventsVar.now(), event) then
          totalEventsVar.update(_ + 1)
          arrivedVar.set(Some(event.eventId))
        liveEventsVar.update(current => mergeEvents(current, event))
      },
      // Clears the highlight, unless a newer event took it
      liveEvent.delay(2000) --> { event =>
        arrivedVar.update(_.filterNot(_ == event.eventId))
      }
    )
  end keepLiveEventsUpToDate

end EventSourcingView
