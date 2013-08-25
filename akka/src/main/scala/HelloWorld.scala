import akka.actor.{Props, ActorSystem, Actor}

/**
 * User: hanlho
 * DateTime: 25/08/13 10:52
 *
 * http://alvinalexander.com/scala/simple-scala-akka-actor-examples-hello-world-actors
 */
object HelloWorld extends App {

  val system = ActorSystem("hello-world-system")
  val helloActor = system.actorOf(Props[HelloActor],"hello-actor")

  helloActor ! "hello"
  helloActor ! "Bonjour"

  class HelloActor extends Actor{
    def receive = {
      case "hello" => println("nice to meet you!")
      case _ => println("* blank stare *")
    }
  }

}
