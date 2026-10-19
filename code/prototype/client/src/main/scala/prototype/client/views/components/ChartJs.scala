package prototype.client.views.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import scala.annotation.unused
import com.raquo.laminar.api.L.*

object chartJS:

  @js.native
  @JSImport("chart.js/auto", JSImport.Default)
  class Chart(@unused ctx: dom.HTMLCanvasElement, @unused config: ChartConfig)
      extends js.Object:
    def update(): Unit = js.native
    def destroy(): Unit = js.native
    var data: ChartData = js.native

  trait ChartDataset extends js.Object:
    val label: String
    val data: js.Array[Double]
    val backgroundColor: js.UndefOr[String] = js.undefined
    val borderColor: js.UndefOr[String] = js.undefined

  trait ChartData extends js.Object:
    val labels: js.Array[String]
    val datasets: js.Array[ChartDataset]

  trait ChartConfig extends js.Object:
    val `type`: String
    val data: ChartData
    val options: js.UndefOr[js.Object] = js.undefined

  def chartCanvas(config: Signal[ChartConfig]): HtmlElement =
    var chart: Option[Chart] = None
    canvasTag(
      onMountUnmountCallbackWithState(
        mount = mountCtx =>
          config.foreach(cfg =>
            chart match
              case Some(c) =>
                c.data = cfg.data
                c.update()
              case None =>
                chart = Some(new Chart(mountCtx.thisNode.ref, copyConfig(cfg)))
          )(using mountCtx.owner),
        unmount = (_, _) =>
          chart.foreach(_.destroy())
          chart = None
      )
    )

  private def copyConfig(config: ChartConfig): ChartConfig =
    js.Object.assign(js.Dynamic.literal(), config).asInstanceOf[ChartConfig]

  // For charts of counts: the y axis starts at 0 and has whole numbers only
  def integerAxisOptions(): js.Object =
    js.Dynamic.literal(
      scales = js.Dynamic.literal(
        y = js.Dynamic.literal(
          beginAtZero = true,
          ticks = js.Dynamic.literal(precision = 0)
        )
      )
    )
