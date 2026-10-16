package prototype.client.views.admin

import com.raquo.laminar.api.L.*

/** A read-only table of text cells. Rows are filtered by the search text (any
  * column, ignoring case), then sorted by the first column and the second,
  * ignoring case. Clicking a row selects it.
  */
object AdminTable:

  def apply[A](
      headers: List[String],
      itemsS: Signal[Option[List[A]]],
      cellsOf: A => List[String],
      onSelect: A => Unit,
      searchS: Signal[String],
      errorS: Signal[Option[String]],
      emptyTextS: Signal[String] = Val("Nothing here yet.")
  ): HtmlElement =
    div(
      child.maybe <-- errorS.map(
        _.map(msg => div(cls := "banner banner--error", msg))
      ),
      div(
        cls := "admin__scroll",
        table(
          cls := "admin__table",
          thead(tr(headers.map(h => th(h)))),
          tbody(
            children <-- itemsS
              .combineWith(searchS, emptyTextS)
              .map { case (items, search, emptyText) =>
                val query = search.trim.toLowerCase
                items match
                  case None =>
                    List(messageRow(headers.size, "Loading…"))
                  case Some(all) =>
                    val rows = all
                      .map(item => item -> cellsOf(item))
                      .filter((_, cells) =>
                        cells.exists(_.toLowerCase.contains(query))
                      )
                      .sortBy((_, cells) => sortKey(cells))
                    if rows.nonEmpty then
                      rows.map((item, cells) => renderRow(item, cells, onSelect))
                    else if query.nonEmpty && all.nonEmpty then
                      List(messageRow(headers.size, "Nothing matches your search."))
                    else List(messageRow(headers.size, emptyText))
              }
          )
        )
      )
    )
  end apply

  private def sortKey(cells: List[String]): (String, String) =
    (
      cells.headOption.fold("")(_.toLowerCase),
      cells.lift(1).fold("")(_.toLowerCase)
    )

  // A row is a button in spirit: it can be reached with the keyboard too
  private def renderRow[A](
      item: A,
      cells: List[String],
      onSelect: A => Unit
  ): HtmlElement =
    tr(
      cls := "admin__row",
      tabIndex := 0,
      cells.map(c => td(c)),
      onClick --> { _ => onSelect(item) },
      onKeyDown.filter(_.key == "Enter") --> { _ => onSelect(item) }
    )

  private def messageRow(columns: Int, message: String): HtmlElement =
    tr(td(cls := "admin__empty", colSpan := columns, message))
end AdminTable
