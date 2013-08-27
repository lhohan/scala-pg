package pi

/**
 * User: hanlho
 * DateTime: 27/08/13 17:45
 *
 * http://doc.akka.io/docs/akka/2.0/intro/getting-started-first-scala.html
 */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

class Pi {

  sealed trait PiMessage

  case object Calculate extends PiMessage

  case class Work(start: Int, nrOfElements: Int) extends PiMessage

  case class Result(value: Double) extends PiMessage

  case class PiApproximation(pi: Double, duration: Duration) extends PiMessage

  class Worker extends Actor {

    // TODO: create functional equivalent
    def calculatePiFor(start: Int, nrOfElements: Int): Double = {
      var acc = 0.0
      for (i <- start until (start + nrOfElements))
        acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
      acc
    }

    def receive = {
      case Work(start, nrOfElements) => sender ! Result(calculatePiFor(start, nrOfElements))

    }
  }

}
