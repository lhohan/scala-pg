package signals

import java.util.UUID

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Input

import scala.scalajs.js

object UI extends js.JSApp {

  val version = UUID.randomUUID().toString


  def main(): Unit = {
    val unique = elementById[html.Div]("version")
    unique.innerHTML = version
    elementById[html.Div]("playground").appendChild(unique)

    setupSquared
  }


  def setupSquared: Unit = {
    val x = elementById[Input]("x")
    val input = Var(x.value)
    
    // here we make the changes on the client are triggering the change in our Signal
    val onChange = { (event: dom.Event) =>
      input() = x.value
    }
    x.addEventListener("change", onChange)
    x.addEventListener("keypress", onChange)
    x.addEventListener("keyup", onChange)
    
    // Link change of our input to the update of the squared element
    Signal {
      elementById[html.Span]("squared").innerHTML = Logic.square(input().toInt).toString
    }
  }


  def elementById[A <: js.Any](id: String): A =
    dom.document.getElementById(id).asInstanceOf[A]

}