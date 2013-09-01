import akka.actor.{ActorRef, Props, Actor, ActorSystem}

/**
 * User: hanlho
 * DateTime: 31/08/13 12:03
 *
 * A contrived code sample that monitors a file location
 * and when files are found they are copied to different location.
 *
 * Playing with akka.
 */
object FileCopier extends App {

  monitor()

  case class StartMonitoring()

  case class FindFiles(location: String)

  case class FileFound(file: String)

  case class CopyFile(file: String, targetLocation: String)

  case class Wait()

  case class Monitor()


  def monitor() {
    val system = ActorSystem("FileCopySystem")
    val fileCopier = system.actorOf(Props(new FileCopier()), name = "file-copier")
    val fileFinder = system.actorOf(Props(new FileFinder(fileCopier)), name = "file-finder")
    val locationMonitor = system.actorOf(Props(new LocationMonitor("file location X", fileFinder)), name = "location-monitor")
    locationMonitor ! StartMonitoring()
  }

  class LocationMonitor(location: String, fileFinder: ActorRef) extends Actor {
    val waitTime: Int = 1000 // ms

    def receive = {
      case Monitor() =>
        fileFinder ! FindFiles(location)
        self ! Wait()
      case Wait() =>
        Thread.sleep(waitTime)
        self ! Monitor()
      case StartMonitoring() =>
        ld("start monitoring location " + location)
        self ! Monitor()
    }
  }

  class FileFinder(fileCopier: ActorRef) extends Actor {

    // TODO temp
    var filesFound = false

    def receive = {
      case FindFiles(location) =>
        ld("checking for files at " + location)
        filesFound = !filesFound
        if (filesFound) {
          ld("found files ")
          fileCopier ! CopyFile("myFile", "targetLocation")
        }
    }
  }

  class FileCopier extends Actor {
    def receive = {
      case CopyFile(file, targetLocation) =>
        ld("copying file " + file + " to " + targetLocation)
    }
  }

  // quick log debug
  def ld(msg: String) = {
    // akka logger?
    println("DEBUG " + msg)
  }

  // quick log info
  def li(msg: String) = {
    // akka logger?
    println("INFO " + msg)
  }


}
