package prototype.client.views.components

import com.raquo.laminar.api.L.*

/** Small line icons, drawn with the text colour (`currentColor`) so a button
  * can colour them. The paths are on a 24 x 24 grid.
  */
object Icons:

  private def icon(paths: String*): SvgElement =
    svg.svg(
      svg.cls := "icon",
      svg.viewBox := "0 0 24 24",
      svg.width := "16",
      svg.height := "16",
      svg.fill := "none",
      svg.stroke := "currentColor",
      svg.strokeWidth := "2",
      svg.strokeLineCap := "round",
      svg.strokeLineJoin := "round",
      aria.hidden := true,
      paths.map(d => svg.path(svg.d := d))
    )

  def trash: SvgElement =
    icon("M3 6h18", "M8 6V4h8v2", "M19 6l-1 14H6L5 6", "M10 11v6", "M14 11v6")

  def check: SvgElement = icon("M20 6L9 17l-5-5")

  def cross: SvgElement = icon("M18 6L6 18", "M6 6l12 12")

  def chevronLeft: SvgElement = icon("M15 18l-6-6 6-6")

  def chevronRight: SvgElement = icon("M9 18l6-6-6-6")

  def chevronFirst: SvgElement = icon("M17 18l-6-6 6-6", "M7 6v12")

  def chevronLast: SvgElement = icon("M7 18l6-6-6-6", "M17 6v12")
