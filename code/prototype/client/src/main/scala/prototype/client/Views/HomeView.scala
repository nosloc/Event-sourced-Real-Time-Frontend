package prototype.client.Views

import com.raquo.laminar.api.L.*

object HomeView:
  def apply(username: String): HtmlElement =
    div(
      cls := "home",
      h1(s"Welcome, $username"),
      p(cls := "muted", "Open Timetable in the top bar to see your week.")
    )
