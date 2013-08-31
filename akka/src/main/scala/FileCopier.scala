import akka.actor.{Props, Actor, ActorSystem}

/**
 * User: hanlho
 * DateTime: 31/08/13 12:03
 *
 * A contrived code sample that monitors a file location
 * and when files are found they are copied to different location.
 *
 * Playing with akka.
 */
object FileCopier extends App{

  monitor()

  case class StartMonitoring(fileLocation: String)

  def monitor() {
    val system = ActorSystem("FileCopySystem")
    val locationMonitor = system.actorOf(Props(new LocationMonitor()), name = "location-monitor")
    locationMonitor ! StartMonitoring("location X")
    locationMonitor ! StartMonitoring("location Y")
  }

  class LocationMonitor extends Actor {
    def receive = {
      case StartMonitoring(fileLocation) =>
        // akka logger?
        println("start monitoring location " + fileLocation)
    }
  }


}
