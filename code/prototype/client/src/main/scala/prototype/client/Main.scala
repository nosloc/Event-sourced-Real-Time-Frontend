package prototype.client

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import prototype.Greeting

@main
def start(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    div(h1(Greeting.greet("Laminar")))
  )
