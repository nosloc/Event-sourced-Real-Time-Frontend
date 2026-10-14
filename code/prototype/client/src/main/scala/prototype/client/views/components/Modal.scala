package prototype.client.views.components

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object Modal:
  def apply(
      titleS: Signal[String],
      onClose: () => Unit
  )(content: Mod[HtmlElement]*): HtmlElement =
    div(
      cls := "modal",
      div(
        cls := "modal__dialog",
        div(
          cls := "modal__head",
          h2(cls := "modal__title", text <-- titleS),
          button(
            cls := "modal__close",
            "×",
            onClick --> { _ =>
              onClose()
            }
          )
        ),
        content
      ),
      onClick.filter(ev => ev.target == ev.currentTarget) --> { _ =>
        onClose()
      },
      // Escape first closes an open popover (a picker list), not the modal
      documentEvents(
        _.onKeyDown.filter(ev =>
          ev.key == "Escape" && dom.document.querySelector(":popover-open") == null
        )
      ) --> { _ =>
        onClose()
      }
    )

  def apply(
      title: String,
      onClose: () => Unit
  )(content: Mod[HtmlElement]*): HtmlElement =
    apply(Val(title), onClose)(content*)
