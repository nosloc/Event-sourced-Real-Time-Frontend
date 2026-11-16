package prototype.client.views.components

import com.raquo.laminar.api.L.*

/** The pieces the step-by-step panel's graphs are drawn with, the same on both
  * levels: the nodes, the events (a coloured dot each), the capsules (a reload,
  * a snapshot, the figures) and the arrows. Each level lays them out in its own
  * coordinates (see stepper.css).
  */
object StepGraph:

  /** A node of the graph, placed by its `modifier` class; the node the current
    * step changed is drawn in amber, with a "now" tag.
    */
  def node(
      modifier: String,
      title: String,
      isNow: Boolean,
      body: Seq[HtmlElement]
  ): HtmlElement =
    div(
      cls := s"stepper__node stepper__node--$modifier" +
        (if isNow then " stepper__node--now" else ""),
      div(
        cls := "stepper__node-head",
        span(cls := "stepper__node-title", title),
        Option.when(isNow)(span(cls := "stepper__now", "now"))
      ),
      div(cls := "stepper__node-body", body)
    )

  /** An event on its own: its dot and its position. */
  def event(position: Long): HtmlElement =
    span(
      cls := "stepper__event",
      dot(position),
      span(cls := "stepper__position", aria.hidden := true, position.toString)
    )

  /** One colour per event, from its position. */
  def dot(position: Long): HtmlElement =
    span(
      cls := s"stepper__dot stepper__dot--${position % 8}",
      role := "img",
      aria.label := s"event $position",
      title := s"event $position"
    )

  /** A capsule holding the dots of `events`, and a note when there is one: a
    * reload, a snapshot, the figures. `modifier` gives its look (pending, in
    * flight, answered, displayed, failed, cancelled).
    */
  def capsule(
      modifier: String,
      name: String,
      events: Seq[Long],
      note: Option[String]
  ): HtmlElement =
    span(
      cls := s"stepper__load stepper__load--$modifier",
      span(cls := "stepper__load-name", name),
      events.sorted.map(dot),
      note.map(text => span(cls := "stepper__load-note", text))
    )

  /** The arrows of a graph, in its own coordinates (`width` by `height`). The
    * `usual` moves are always drawn, the `occasional` ones only when lit; the
    * `lit` ones (the current step's) are amber, drawn last so that they sit on
    * top, with their label when `label` gives one. `idPrefix` keeps the arrow
    * heads' ids apart from another graph's.
    */
  def arrows[M](
      width: Int,
      height: Int,
      idPrefix: String,
      usual: List[M],
      occasional: List[M],
      lit: Set[M],
      path: M => String,
      label: M => Option[(String, Int, Int)]
  ): SvgElement =
    val (now, others) =
      (usual ++ occasional.filter(lit.contains)).partition(lit.contains)
    val head = s"$idPrefix-arrow-head"
    val headNow = s"$idPrefix-arrow-head-now"

    def marker(id: String, modifier: String) =
      svg.marker(
        svg.idAttr := id,
        svg.viewBox := "0 0 8 8",
        svg.refX := "7",
        svg.refY := "4",
        svg.markerWidth := "8",
        svg.markerHeight := "8",
        svg.orient := "auto",
        svg.path(svg.d := "M0 0 8 4 0 8z", svg.cls := modifier)
      )

    def arrow(move: M, isNow: Boolean) =
      svg.path(
        svg.cls := (if isNow then "stepper__arrow stepper__arrow--now"
                    else "stepper__arrow"),
        svg.d := path(move),
        svg.markerEnd := "url(#" + (if isNow then headNow else head) + ")"
      )

    svg.svg(
      svg.cls := "stepper__arrows",
      svg.width := width.toString,
      svg.height := height.toString,
      svg.viewBox := s"0 0 $width $height",
      svg.fill := "none",
      aria.hidden := true,
      svg.defs(
        marker(head, "stepper__arrow-head"),
        marker(headNow, "stepper__arrow-head stepper__arrow-head--now")
      ),
      others.map(arrow(_, false)),
      now.map(arrow(_, true)),
      now
        .flatMap(label)
        .map((text, x, y) =>
          svg.text(
            svg.cls := "stepper__arrow-label",
            svg.x := x.toString,
            svg.y := y.toString,
            text
          )
        )
    )
