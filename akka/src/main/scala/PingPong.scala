import akka.actor.{Props, ActorRef, Actor, ActorSystem}

/**
 * User: hanlho
 * DateTime: 25/08/13 11:37
 */
object PingPong extends App {

  case class StartMessage()

  case class StopMessage()

  case class PingMessage()

  case class PongMessage()

  val system = ActorSystem("pingpong-system")
  val pong = system.actorOf(Props[Pong], name = "pong")
  val ping = system.actorOf(Props(new Ping(pong)), name = "ping")

  ping ! StartMessage

  class Ping(pong: ActorRef) extends Actor {

    var count = 0

    def incrementAndPing: Unit = {
      count += 1
      println("ping " + count)
    }

    def receive = {
      case StartMessage => {
        incrementAndPing
        pong ! PingMessage
      }
      case PongMessage => {
        incrementAndPing
        if (count > 99) {
          sender ! StopMessage
          println("ping stopped")
          context.stop(self)
        } else {
          sender ! PingMessage
        }
      }

    }
  }

  class Pong extends Actor {
    def receive = {
      case PingMessage => {
        println("pong")
        sender ! PongMessage
      }
      case StopMessage => {
        println("Pong stopped")
        context.stop(self)
      }
    }
  }

}
