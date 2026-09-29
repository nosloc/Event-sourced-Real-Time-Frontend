package livechart

import com.raquo.laminar.api.L.{*, given}
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.util.Random

import org.scalajs.dom

// import javascriptLogo from "/javascript.svg"
@js.native @JSImport("/javascript.svg", JSImport.Default)
val javascriptLogo: String = js.native

@main
def LiveChart(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )
end LiveChart

object Main:
  val model = new Model
  import model.*

  def appElement(): Element =
    div(
      h1("Live Chart"),
      renderDataTable(),
      renderDataList()
    )
  end appElement

  def renderDataTable(): Element =
    table(
      thead(
        tr(
          th("Label"),
          th("Price"),
          th("Count"),
          th("Full Price"),
          th("Actions")
        )
      ),
      tbody(
        children <-- dataSignal.split(_.id)((id, initial, itemSignal) =>
          renderDataItem(id, itemSignal)
        )
      ),
      tfoot(
        tr(
          button("➕", onClick --> (_ => addItem(DataItem()))),
          td(),
          td(),
          td(
            child.text <-- dataSignal.map(data =>
              "%.2f".format(data.map(_.fullPrice).sum)
            )
          )
        )
      )
    )
  end renderDataTable

  def renderDataItem(id: DataItemID, item: Signal[DataItem]): Element =
    tr(
      td(
        inputForString(
          item.map(_.label),
          makeDataItemUpdater[String](
            id,
            (item, newLabel) => item.copy(label = newLabel)
          )
        )
      ),
      td(
        inputForDouble(
          item.map(_.price),
          makeDataItemUpdater[Double](
            id,
            (item, newPrice) => item.copy(price = newPrice)
          )
        )
      ),
      td(
        inputForInt(
          item.map(_.count),
          makeDataItemUpdater[Int](
            id,
            (item, newCount) => item.copy(count = newCount)
          )
        )
      ),
      td(child.text <-- item.map(x => "%.2f".format(x.fullPrice))),
      td(button("🗑️", onClick --> (_ => removeItem(id))))
    )
  end renderDataItem

  def inputForDouble(
      valueSignal: Signal[Double],
      valueUpdater: Observer[Double]
  ): Input =
    val strValue = Var[String]("")
    input(
      tpe := "text",
      value <-- strValue.signal,
      onInput.mapToValue --> strValue,
      valueSignal --> strValue.updater[Double] { (prevStr, newValue) =>
        if prevStr.toDoubleOption.contains(newValue) then prevStr
        else newValue.toString
      },
      strValue.signal --> (valueStr =>
        valueStr.toDoubleOption.foreach(valueUpdater.onNext)
      )
    )

  def inputForString(
      valueSignal: Signal[String],
      valueUpdater: Observer[String]
  ): Input =
    input(
      tpe := "text",
      value <-- valueSignal,
      onInput.mapToValue --> valueUpdater
    )
  end inputForString

  def inputForInt(
      valueSignal: Signal[Int],
      valueUpdater: Observer[Int]
  ): Input =
    input(
      typ := "text",
      controlled(
        value <-- valueSignal.map(_.toString),
        onInput.mapToValue.map(_.toIntOption).collect { case Some(newCount) =>
          newCount
        } --> valueUpdater
      )
    )

  def makeDataItemUpdater[A](
      id: DataItemID,
      f: (DataItem, A) => DataItem
  ): Observer[A] =
    dataVar.updater[A]((data, newValue) =>
      data.map(item => if item.id == id then f(item, newValue) else item)
    )

  def renderDataList(): Element =
    ul(
      children <-- dataSignal.split(_.id)((id, initial, itemSignal) =>
        li(
          child.text <-- itemSignal.map(item =>
            s"${item.count} x ${item.label}"
          )
        )
      )
    )
  end renderDataList

  def counterButton(): Element =
    val counter = Var(0)
    button(
      tpe := "button",
      "count is ",
      child.text <-- counter,
      onClick --> { event => counter.update(_ + 1) }
    )
  end counterButton

end Main
