
import akka.actor._

import FileProcessor._
import FileProcessor.Monitor
import FileProcessor.StartMonitoring
import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger
import StandardWatchEventKinds._
import scala.collection.JavaConversions._

/**
 * User: hanlho
 * DateTime: 31/08/13 12:03
 *
 * Playing with akka.
 *
 * A contrived code sample that monitors a file location
 * and when files are found they are copied/moved to different location.
 *
 * Usage:
 * args:
 * - Pass the dir location to monitor as first argument
 *
 * Example:
 * sbt "run-main LocationMonitorMain src/test/resources/file-copy-test-dir/src target"
 *
 */

object LocationMonitorMain extends App {

  val monitorLocation = Paths.get("src/test/resources/file-copy-test-dir/src")
  val targetLocation = Paths.get("target")
  val processType = Copy
  //  val processType = Move

  val processor = new FileProcessor(monitorLocation, targetLocation, processType)

  processor.monitor()

}

object FileProcessor {

  val system = ActorSystem("FileCopySystem")
  val fileCopier = system.actorOf(Props(new FileProcessor()), name = "file-copier")
  val monitorCounter = new AtomicInteger()

  case class StartMonitoring()

  sealed trait FileCommand

  case class CopyFile(file: Path, target: Path) extends FileCommand

  case class MoveFile(file: Path, target: Path) extends FileCommand

  case class Monitor()


  sealed trait ProcessingType {
    def command(fileToProcess: Path, target: Path): FileCommand
  }

  case object Move extends ProcessingType {
    override def command(file: Path, target: Path): FileCommand = {
      new MoveFile(file, target)
    }
  }

  case object Copy extends ProcessingType {
    override def command(file: Path, target: Path): FileCommand = {
      new CopyFile(file, target)
    }
  }

  class FileProcessor() extends Actor {
    def receive = {
      case CopyFile(file, targetLocation) =>
        ld("copying file " + file.getFileName + " to " + targetLocation)
        copyFileToLocation(file, targetLocation)
      case MoveFile(file, targetLocation) =>
        ld("moving file " + file.getFileName + " to " + targetLocation)
        FileProcessor.moveFileToLocation(file, targetLocation)
    }
  }


  def copyFileToLocation(file: Path, targetLocation: Path) = {
    Files.copy(file, targetLocation.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING)
  }

  def moveFileToLocation(file: Path, targetLocation: Path) = {
    Files.move(file, targetLocation.resolve(file.getFileName), StandardCopyOption.ATOMIC_MOVE)
  }


  // logging
  val IsDebugEnabled = false
  val IsInfoEnabled = true

  // quick log debug
  def ld(msg: String) = {
    // akka logger?
    if (IsDebugEnabled) println("DEBUG " + msg)
  }

  // quick log info
  def li(msg: String) = {
    // akka logger?
    if (IsInfoEnabled) println("INFO " + msg)
  }


}

class FileProcessor(val monitorLocation: Path, val targetLocation: Path, val processingType: ProcessingType) {


  if (!Files.isDirectory(monitorLocation)) {
    throw new IllegalArgumentException("Location to monitor [" + monitorLocation + "]should be directory.")
  }
  if (!Files.isDirectory(targetLocation)) {
    throw new IllegalArgumentException("Target location [" + targetLocation + "]should be directory.")
  }


  def monitor() {
    val locationMonitor = system.actorOf(Props(
      new LocationMonitor(monitorLocation, targetLocation, processingType, fileCopier)
    ), name = "locationMonitor-" + monitorCounter.incrementAndGet())
    locationMonitor ! StartMonitoring()
  }

  class LocationMonitor(location: Path, targetLocation: Path, processingType: ProcessingType, fileProcessor: ActorRef) extends Actor {
    val watcher = FileSystems.getDefault().newWatchService()
    location.register(watcher, ENTRY_CREATE)

    def receive = {
      case Monitor() =>
        val watchKey = watcher.take()
        for (event <- watchKey.pollEvents()) {
          val path = event.context().asInstanceOf[Path]
          ld("watcher found: " + path)
          fileProcessor ! processingType.command(location.resolve(path), targetLocation)
        }
        watchKey.reset()
        self ! Monitor()
      case StartMonitoring() =>
        li("start monitoring location " + location)
        self ! Monitor()
    }
  }

}