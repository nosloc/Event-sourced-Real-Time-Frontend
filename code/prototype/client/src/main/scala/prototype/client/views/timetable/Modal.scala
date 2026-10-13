package prototype.client.views.timetable

import com.raquo.laminar.api.L.*

object Modal:
  def apply(
      title: String,
      onClose: () => Unit
  )(content: Mod[HtmlElement]*): HtmlElement =
    div(
      cls := "modal",
      div(
        cls := "modal__dialog",
        div(
          cls := "modal__head",
          h2(cls := "modal__title", title),
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
      documentEvents(_.onKeyDown.filter(_.key == "Escape")) --> { _ =>
        onClose()
      }
    )
