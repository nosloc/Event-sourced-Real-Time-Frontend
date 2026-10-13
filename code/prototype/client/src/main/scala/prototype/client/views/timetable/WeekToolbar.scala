package prototype.client.views.Timetable

import com.raquo.laminar.api.L.*
import prototype.client.views.Timetable.TimetableModel.*
import scala.scalajs.js.Date

object WeekToolbar:
  def apply(
      weekRangeS: Signal[WeekRange],
      weekOffsetVar: Var[Int]
  ): HtmlElement =
    div(
      cls := "timetable__toolbar",
      renderDateRange(weekRangeS.map(_.monday), weekRangeS.map(_.sunday)),
      div(
        cls := "btn-group",
        changingWeekButton("‹", weekOffsetVar, -1),
        todayButton(weekOffsetVar),
        changingWeekButton("›", weekOffsetVar, 1)
      )
    )
  end apply

  private def renderDateRange(
      mondayS: Signal[Date],
      sundayS: Signal[Date]
  ): HtmlElement =
    div(
      cls := "timetable__range",
      child.text <-- mondayS.combineWith(sundayS).map { case (monday, sunday) =>
        s"Week from ${monday.dateToString} to ${sunday.dateToString}"
      }
    )
  end renderDateRange

  private def changingWeekButton(
      label: String,
      offsetVar: Var[Int],
      weekOffset: Int
  ): HtmlElement =
    button(
      cls := "btn btn--icon",
      label,
      onClick --> { _ =>
        offsetVar.update(_ + weekOffset)
      }
    )
  end changingWeekButton

  private def todayButton(offsetVar: Var[Int]): HtmlElement =
    button(
      cls := "btn",
      "Today",
      onClick --> { _ =>
        offsetVar.set(0)
      },
      disabled <-- offsetVar.signal.map(_ == 0)
    )
  end todayButton
