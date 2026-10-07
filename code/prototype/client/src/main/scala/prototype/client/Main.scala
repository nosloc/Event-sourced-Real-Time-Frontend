package prototype.client

import com.raquo.laminar.api.L.{*, given}
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
