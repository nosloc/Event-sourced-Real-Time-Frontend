package prototype.client

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import prototype.client.Views.View

@main
def start(): Unit =

  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    div(
      child <-- View.views,
      View.changeUrlOnDiffer
    )
  )
