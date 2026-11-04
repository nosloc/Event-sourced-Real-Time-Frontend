package prototype.client

import prototype.client.views.View

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main
def start(): Unit =

  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    div(
      child <-- View.views,
      View.changeUrlOnDiffer
    )
  )
