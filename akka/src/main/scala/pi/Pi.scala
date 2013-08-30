/**
 * User: hanlho
 * DateTime: 27/08/13 17:45
 *
 * http://doc.akka.io/docs/akka/2.0/intro/getting-started-first-scala.html
 *
 * Run using sbt directly as:
 *    sbt "run-main Pi"
 */

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

object Pi extends App {

  calculate(nrOfWorkers = 40, nrOfElements = 1000, nrOfMessages = 2000000)

  sealed trait PiMessage

  case object Calculate extends PiMessage

  case class Work(start: Int, nrOfElements: Int) extends PiMessage

  case class Result(value: Double) extends PiMessage

  case class PiApproximation(pi: Double, duration: Duration) extends PiMessage

  class Worker extends Actor {

    // TODO: write test and create functional equivalent
    def calculatePiFor(start: Int, nrOfElements: Int): Double = {
      var acc = 0.0
      for (i <- start until (start + nrOfElements))
        acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
      acc
    }

    def receive = {
      case Work(start, nrOfElements) =>
        sender ! Result(calculatePiFor(start, nrOfElements))
    }

  }

  class Master(nrOfWorkers: Int, nrOfMessages: Int, nrOfElements: Int, listener: ActorRef) extends Actor {

    var pi: Double = _
    var nrOfResults: Int = _
    val start: Long = System.currentTimeMillis

    val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workRouter")

    def receive = {
      case Calculate => {
        for (i <- 0 until nrOfMessages) workerRouter ! Work(i * nrOfElements, nrOfElements)
      }
      case Result(value) => {
        pi += value
        nrOfResults += 1
        if (nrOfResults == nrOfMessages) {
          listener ! PiApproximation(pi, duration = (System.currentTimeMillis() - start).millis)
          context.stop(self)
        }
      }
    }
  }

  class Listener extends Actor {
    def receive = {
      case PiApproximation(pi, duration) => {
        println("\n\tPi approximation: \t\t%s\n\tCalculation time: \t%s"
          .format(pi, duration))
        context.system.shutdown()
      }
    }
  }

  def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessages: Int) {
    // create an Akka system
    val system = ActorSystem("PiSystem")

    // create the result listener, which will print the result and shuts down the system
    val listener = system.actorOf(Props[Listener], name = "listener")

    // create master
    val master = system.actorOf(Props(new Master(
      nrOfWorkers, nrOfMessages, nrOfElements, listener
    )), name = "master")

    master ! Calculate
  }


}
