import akka.actor.Actor

/**
 * User: hanlho
 * DateTime: 26/12/13 15:16
 */
class Toggle extends Actor {
  def happy: Receive = {
    case "Hello, how are you?" =>
      sender ! "Happy"
      context become (sad)
  }

  def sad: Receive = {
    case "Hello, how are you?" =>
      sender ! "Sad"
      context become (happy)
  }

  def receive = happy

}
