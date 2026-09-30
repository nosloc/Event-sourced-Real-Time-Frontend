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
    val counter: Var[Int] = Var(0)
    val counterSignal = counter.signal
    div(
      h1("Counter Playground"),
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

  def renderButton(
      text: String,
      counter: Var[Int],
      f: (Int) => (Int)
  ): Element =
    button(text, onClick --> (_ => counter.update(f)))
