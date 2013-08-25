import akka.actor.{Props, ActorSystem, Actor}

/**
 * User: hanlho
 * DateTime: 25/08/13 10:52
 *
 * http://alvinalexander.com/scala/simple-scala-akka-actor-examples-hello-world-actors
 */
object HelloWorld extends App {

  val system = ActorSystem("hello-world-system")
  val helloActor = system.actorOf(Props(new HelloActor("Hans")),"hello-actor")

  helloActor ! "hello"
  helloActor ! "Bonjour"

  system.shutdown()

  class HelloActor(name: String) extends Actor{
    def receive = {
      case "hello" => println("%s: nice to meet you!".format(name))
      case _ => println("%s: * blank stare *".format(name))
    }
  }

}
