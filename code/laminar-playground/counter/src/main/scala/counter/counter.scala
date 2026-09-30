package counter

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom

import com.raquo.laminar.api.L.{*, given}

@main
def counter: Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.counterElement()
  )

object Main:
  def counterElement(): Element =
    div(
      h1("Counter Playground"),
      renderManualCounter(),
      renderAutomaticCounter(),
      renderEventSourcedCounter()
    )

  def renderManualCounter(): Element =
    val counter: Var[Int] = Var(0)
    val counterSignal = counter.signal
    div(
      h2("Manual Counter"),
      p("The counter is at: ", child.text <-- counterSignal),
      div(
        cls := "button-row",
        renderButton("Add", counter, x => x + 1),
        renderButton("Minus", counter, x => x - 1),
        renderButton("Reset", counter, _ => 0)
      )
    )

  def renderAutomaticCounter(): Element =
    val timer: EventStream[Int] =
      EventStream.periodic(1000)
    val automaticCounter: Var[Int] = Var(0)
    div(
      h2("Automatic Counter"),
      p(
        "The automatic counter is at: ",
        child.text <-- automaticCounter.signal
      ),
      renderButton("Reset", automaticCounter, _ => 0),
      timer --> (_ => {
        automaticCounter.update(_ + 1)
      }),
      timer --> (_ => dom.console.log("Tick!"))
    )

  sealed trait Event
  case class Increment(by: Int) extends Event
  case class Decrement(by: Int) extends Event
  case object Reset extends Event

  case class LoggedEvent(id: Int, event: Event)

  class EventLog:
    private val eventBus = new EventBus[Event]
    val eventStream = eventBus.events
    val logSignal =
      eventStream.scanLeft(List.empty[LoggedEvent])((prev, e) =>
        prev :+ LoggedEvent(prev.length, e)
      )

    def addEvent(event: Event) =
      eventBus.emit(event)

  def renderEventSourcedCounter(): Element =
    val eventLog = new EventLog
    val eventSourcedCounter: Var[Int] = Var(0)
    div(
      h2("Event sourced counter"),
      p(
        "The event sourced button is at: ",
        child.text <-- eventSourcedCounter.signal
      ),
      eventLog.eventStream --> (handleEvent(eventSourcedCounter, _)),
      div(
        cls := "button-row",
        renderEventButton("Add", Increment(1), eventLog),
        renderEventButton("Minus", Decrement(1), eventLog),
        renderEventButton("Reset", Reset, eventLog)
      ),
      table(
        thead(
          tr(
            th("Number"),
            th("Event")
          )
        ),
        tbody(
          children <-- eventLog.logSignal.split(_.id) { (_, initial, _) =>
            renderRow(initial)
          }
        )
      )
    )

  def handleEvent(counter: Var[Int], event: Event): Unit =
    event match
      case Increment(by) => counter.update(_ + by)
      case Decrement(by) => counter.update(_ - by)
      case Reset         => counter.set(0)

  def renderRow(event: LoggedEvent): Element =
    event match
      case LoggedEvent(index, Increment(by)) =>
        tr(td(index), td(s"Increment($by)"))
      case LoggedEvent(index, Decrement(by)) =>
        tr(td(index), td(s"Decrement($by)"))
      case LoggedEvent(index, Reset) =>
        tr(td(index), td(s"Reset"))

  def renderEventButton(
      text: String,
      eventEmited: Event,
      eventLog: EventLog
  ): Element =
    button(
      text,
      onClick --> (_ => eventLog.addEvent(eventEmited))
    )

  def renderButton[A](
      text: String,
      variable: Var[A],
      f: A => A
  ): Element =
    button(text, onClick --> (_ => variable.update(f)))
