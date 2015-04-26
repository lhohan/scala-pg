package signals

import java.util.UUID

import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Input, Span}
import org.scalajs.dom.{CanvasRenderingContext2D, Event}

import scala.scalajs.js

object UI extends js.JSApp {

  val version = UUID.randomUUID().toString


  def main(): Unit = {
    //    val unique = elementById[html.Div]("version")
    //    unique.innerHTML = version

    setUpCanvas()
  }


  def setUpCanvas(): Unit = {
    val (canvas: Canvas, renderer: CanvasRenderingContext2D) = getCanvas

    renderer.fillStyle = "#f8f8f8"
    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight
    renderer.fillRect(0, 0, canvas.width, canvas.height)

    val input1 = signalElement("input1")
    val input2 = signalElement("input2")

    Signal {
      renderer.clearRect(
        0, 0, canvas.width, canvas.height
      )

      renderer.textAlign = "center"
      renderer.textBaseline = "middle"
      renderer.font = "75px sans-serif"
      renderer.fillStyle = input2()
      renderer.fillText(
        input1(),
        canvas.width / 2,
        canvas.height / 2
      )
    }
  }


  def getCanvas: (Canvas, CanvasRenderingContext2D) = {
    val canvas = elementById[Canvas]("mycanvas")
    val renderer = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    (canvas, renderer)
  }

  def setupSquared: Unit = {
    val input: Var[String] = signalElement("x")

    // Link change of our input to the update of the squared element
    Signal {
      elementById[Span]("squared").innerHTML = Logic.square(input().toInt).toString
    }
  }


  def signalElement(id: String): Var[String] = {
    val x = elementById[Input](id)
    val input = Var(x.value)

    // here we make the changes on the client are triggering the change in our Signal
    val onChange = { (event: Event) =>
//      println(s"value changed to ${x.value}")
      input() = x.value
    }
    x.addEventListener("change", onChange)
    x.addEventListener("keypress", onChange)
    x.addEventListener("keyup", onChange)
    input
  }

  def elementById[A <: js.Any](id: String): A =
    dom.document.getElementById(id).asInstanceOf[A]

}