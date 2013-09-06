import akka.actor.{ActorRef, Props, ActorSystem, Actor}

import java.nio.file._
import StandardWatchEventKinds._
import scala.collection.JavaConversions._

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
 * sbt "run-main FileProcessor src/test/resources/file-copy-test-dir/src target"
 *
 */
object FileProcessor extends App {

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

    val processType = Copy
//  val processType = Move

  monitor()

  case class StartMonitoring()

  sealed trait FileCommand

  case class CopyFile(file: Path) extends FileCommand

  case class MoveFile(file: Path) extends FileCommand

  case class Monitor()


  def monitor() {
    val system = ActorSystem("FileCopySystem")
    val fileCopier = system.actorOf(Props(new FileProcessor(targetLocation)), name = "file-copier")
    val locationMonitor = system.actorOf(Props(new LocationMonitor(monitorLocation, fileCopier)), name = "location-monitor")
    locationMonitor ! StartMonitoring()
  }

  class LocationMonitor(location: Path, fileProcessor: ActorRef) extends Actor {
    val watcher = FileSystems.getDefault().newWatchService()
    location.register(watcher, ENTRY_CREATE)

    def receive = {
      case Monitor() =>
        val watchKey = watcher.take()
        for (event <- watchKey.pollEvents()) {
          val path = event.context().asInstanceOf[Path]
          ld("watcher found: " + path)
          fileProcessor ! processType.command(location.resolve(path))
        }
        watchKey.reset()
        self ! Monitor()
      case StartMonitoring() =>
        li("start monitoring location " + location)
        self ! Monitor()
    }
  }


  class FileProcessor(targetLocation: Path) extends Actor {
    def receive = {
      case CopyFile(file) =>
        ld("copying file " + file.getFileName + " to " + targetLocation)
        copyFileToLocation(file, targetLocation)
      case MoveFile(file) =>
        ld("moving file " + file.getFileName + " to " + targetLocation)
        FileProcessor.moveFileToLocation(file, targetLocation)
    }
  }

  sealed trait ProcessingType {
    def command(file: Path): FileCommand
  }

  case object Move extends ProcessingType {
    override def command(file: Path): FileCommand = {
      new MoveFile(file)
    }
  }

  case object Copy extends ProcessingType {
    override def command(file: Path): FileCommand = {
      new CopyFile(file)
    }
  }


  def copyFileToLocation(file: Path, targetLocation: Path) = {
    Files.copy(file, targetLocation.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING)
  }

  def moveFileToLocation(file: Path, targetLocation: Path) = {
    Files.move(file, targetLocation.resolve(file.getFileName), StandardCopyOption.ATOMIC_MOVE)
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