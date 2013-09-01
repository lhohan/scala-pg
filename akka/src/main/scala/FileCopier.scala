import akka.actor.{ActorRef, Props, Actor, ActorSystem}
import java.io.File

/**
 * User: hanlho
 * DateTime: 31/08/13 12:03
 *
 * Playing with akka.
 *
 * A contrived code sample that monitors a file location
 * and when files are found they are copied to different location.
 *
 * Usage:
 *  args:
 *    - Pass the dir location to monitor as first argument
 *
 * Example:
 *     sbt "run-main FileCopier target/scala-2.10/test-classes/file-copy-test-dir/src"
 *
 */
object FileCopier extends App {

  if (args.length == 0) {
    throw new IllegalArgumentException("Please pass location to monitor as first argument.")
  }
  val monitorLocation = args(0)

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
    val locationMonitor = system.actorOf(Props(new LocationMonitor(monitorLocation, fileFinder)), name = "location-monitor")
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

  object FileFinder {
    def findFilesAt(location: String): Array[String] = {
      new File(location).list()
    }
  }

  class FileFinder(fileCopier: ActorRef) extends Actor {

    def receive = {
      case FindFiles(location) =>
        ld("checking for files at " + location)
        val filesFound = FileFinder.findFilesAt(location)
        if (!filesFound.isEmpty) {
          for (ff <- filesFound) {
            ld("found file " + ff)
            fileCopier ! CopyFile(ff, "targetLocation")
          }
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
