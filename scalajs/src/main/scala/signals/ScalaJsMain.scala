package signals

import java.util.UUID

import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Input, Span}
import org.scalajs.dom.{CanvasRenderingContext2D, Event}

import scala.scalajs.js
import scala.util.{Try, Random}

object UI extends js.JSApp {

  val version = UUID.randomUUID().toString
  var coords: Seq[(Int, Int)] = _
  val (canvas: Canvas, renderer: CanvasRenderingContext2D) = getCanvas
  // current position is wrapped in a Var which means every signal depending on it will be signalled and be executed
  var positions: Var[List[(Int, Int)]] = _
  var unit: Int = _


  def keepInCanvas(position: (Int, Int)): (Int, Int) = {
    val x = position._1
    val y = position._2
    val x_ = if (x < 0) x + canvas.width else if (x > canvas.width) x - canvas.width else x
    val y_ = if (y < 0) y + canvas.height else if (y > canvas.height) y - canvas.height else y
    (x_, y_)
  }

  def main(): Unit = {
    setUpCanvas()

    // on every run the position is changed
    def run() = {
      val list = positions()
      val left = Random.nextBoolean()
      val down = Random.nextBoolean()
      positions() = list.map { p =>
        keepInCanvas(
          (left, down) match {
            case (true, true)   => (p._1 - unit, p._2 - unit)
            case (true, false)  => (p._1 - unit, p._2 + unit)
            case (false, false) => (p._1 + unit, p._2 + unit)
            case (false, true)  => (p._1 + unit, p._2 - unit)
          }
        )
      }
    }

    dom.setInterval(run _, 200)
  }


  def setUpCanvas(): Unit = {
    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight
    renderer.fillRect(0, 0, canvas.width, canvas.height)
    val heightInUnits = 20
    unit = canvas.height / heightInUnits

    coords = {
      for {
        y <- (unit / 2) until canvas.height by unit
        x <- (unit / 2) until canvas.width by unit
      } yield (x, y)
    }


    val input1 = signalElement("input1")
    val input2 = signalElement("input2")

    def randomCoords(n: Int) = (for {
      _ <- 0 until n
    } yield coords(Random.nextInt(coords.size))).toList

    positions = Var(randomCoords(input1().toInt))

    // here we register the signal that will rerender if one of its dependencies changes!
    Signal {
      
      def drawGrid() = coords.foreach { c =>
        renderer.beginPath()
        renderer.fillStyle = "black"
        renderer.arc(c._1, c._2, 1, 0, 2 * Math.PI)
        renderer.fill()
        renderer.stroke()
      }
      
      def adjustPositions(input1: String) = {
        val valueInput1 = Try(input1.toInt).getOrElse(0)
        val currentInput1 = positions().size
        valueInput1 match {
          case 0 => // do nothing
          case newInput1: Int if newInput1 < currentInput1 =>
            val list = positions()
            positions() = (0 until (currentInput1 - newInput1)).foldLeft(list)((ps, _) => ps.tail)
          case s: Int if s > currentInput1 =>
            val list = positions()
            positions() = list ++ randomCoords(s - currentInput1)
          case _                  => // do nothing
        }
      }
      
      def drawPositions(input2:String) = {
        val list = positions()
        list.foreach { p =>
          renderer.beginPath()
          renderer.arc(p._1, p._2, unit / 2, 0, 2 * Math.PI)
          renderer.fillStyle = input2
          renderer.fill()
          renderer.stroke()
        }
      }
      
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      adjustPositions(input1())
      drawGrid()
      drawPositions(input2())
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