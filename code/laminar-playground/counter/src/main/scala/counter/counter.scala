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
      renderAutomaticCounter()
    )

  def renderManualCounter(): Element =
    val counter: Var[Int] = Var(0)
    val counterSignal = counter.signal
    div(
      h2("Manual Counter"),
      p("The counter is at: ", child.text <-- counterSignal),
      table(
        tbody(
          tr(
            renderButton("Add", counter, x => x + 1),
            renderButton("Minus", counter, x => x - 1),
            renderButton("Reset", counter, _ => 0)
          )
        )
      )
    )

  def renderAutomaticCounter(): Element =
    val timer: EventStream[Unit] =
      EventStream.periodic(1000).mapTo(dom.console.log("Tick!"))
    val automaticCounter: Var[Int] = Var(0)
    div(
      h2("Automatic Counter"),
      p("The automatic counter is at: ", child.text <-- automaticCounter),
      button("Reset", onClick --> (_ => automaticCounter.set(0))),
      timer --> (_ => {
        automaticCounter.update(_ + 1)
      })
    )

  def renderButton(
      text: String,
      counter: Var[Int],
      f: (Int) => (Int)
  ): Element =
    button(text, onClick --> (_ => counter.update(f)))
