import akka.actor.{ActorRef, Props, Actor, ActorSystem}
import java.io.{File, FileOutputStream, FileInputStream}
import java.nio.file._

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
 * args:
 * - Pass the dir location to monitor as first argument
 *
 * Example:
 * sbt "run-main FileCopier src/test/resources/file-copy-test-dir/src target"
 *
 */
object FileCopier extends App {

  if (args.length < 2) {
    throw new IllegalArgumentException("Please pass location to monitor as first argument and/or target location as second.")
  }

  val monitorLocation = Paths.get(args(0))
  val targetLocation = Paths.get(args(1))
  if (!Files.isDirectory(monitorLocation)) {
    throw new IllegalArgumentException("Location to monitor should be directory.")
  }
  if (!Files.isDirectory(targetLocation)) {
    throw new IllegalArgumentException("Target location should be directory.")
  }

  monitor()

  case class StartMonitoring()

  case class FindFiles(location: Path)

  case class FileFound(file: String)

  case class CopyFile(file: Path)

  case class Wait()

  case class Monitor()


  def monitor() {
    val system = ActorSystem("FileCopySystem")
    val fileCopier = system.actorOf(Props(new FileCopier(targetLocation)), name = "file-copier")
    val fileFinder = system.actorOf(Props(new FileFinder(fileCopier)), name = "file-finder")
    val locationMonitor = system.actorOf(Props(new LocationMonitor(monitorLocation, fileFinder)), name = "location-monitor")
    locationMonitor ! StartMonitoring()
  }

  class LocationMonitor(location: Path, fileFinder: ActorRef) extends Actor {
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
    def findFilesAt(location: Path): DirectoryStream[Path] = {
      Files.newDirectoryStream(location)
    }
  }

  class FileFinder(fileCopier: ActorRef) extends Actor {

    def receive = {
      case FindFiles(location) =>
        ld("checking for files at " + location)
        val filesFound = FileFinder.findFilesAt(location)
        val it = filesFound.iterator()
        while (it.hasNext) {
          fileCopier ! CopyFile(it.next())
        }
    }
  }

  object FileCopier {
    def copyFileToLocation(file: Path, targetLocation: Path) = {
      Files.copy(file, targetLocation.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING)
    }
  }

  object FileMover {
    def moveFileToLocation(file: Path, targetLocation: Path) = {
      Files.move(file, targetLocation.resolve(file.getFileName), StandardCopyOption.ATOMIC_MOVE)
    }
  }

  class FileCopier(targetLocation: Path) extends Actor {
    def receive = {
      case CopyFile(file) =>
        ld("copying file " + file.getFileName + " to " + targetLocation)
                FileCopier.copyFileToLocation(file, targetLocation)
//        FileMover.moveFileToLocation(file, targetLocation)
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
