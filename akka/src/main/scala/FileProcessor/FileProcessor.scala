
import akka.actor._

import FileProcessor._
import FileProcessor.Monitor
import FileProcessor.StartMonitoring
import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
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
 *
 * Example:
 * sbt "run-main LocationMonitorMain"
 *
 */

object LocationMonitorMain extends App {


  //  val configs = testSetup
  val configs = readConfigFromFile

  for (config <- configs) {
    new FileProcessor(config).monitor()
  }

  def readConfigFromFile: List[MonitorConfig] = {

    def toProcessingType(processingTypeString: String): ProcessingType = processingTypeString.toLowerCase.trim match {
      case "copy" => Copy
      case "move" => Move
      case _ => throw new IllegalArgumentException("Unknown processing type: " + processingTypeString + ". Should be 'copy, 'move', ..")
    }

    val configFile = {
      if (args.size > 0) {
        Paths.get(args(0))
      } else {
        Paths.get("monitor.config")
      }
    }

    if (!Files.exists(configFile)) {
      throw new IllegalArgumentException("Monitor config file is missing. " +
        "Either provide an existing file as argument or create 'monitor.config'.")
    }

    //TODO : try to improve :

    val lines: Iterator[String] = Source.fromFile(configFile.toFile).getLines()
      .map{_.trim}
      .filter{!_.isEmpty}
      .filter{!_.startsWith("#") }

    val configs = new ArrayBuffer[MonitorConfig]()
    for (configLine <- lines) {
        ld("reading config line: " + configLine)
          val config: Array[String] = configLine.split(",")
          try {
            configs += new MonitorConfig(config(1), config(2), toProcessingType(config(0)))
          } catch {
            case e: Throwable =>
              li("Error reading config line: \n" + configLine + "\n => Error is: " + e.getMessage)
          }
    }
    configs.toList
  }

  def testSetup: List[MonitorConfig] = {
    val config1: MonitorConfig = new MonitorConfig(
      "src/test/resources/file-copy-test-dir/src",
      "target",
      Copy
    )

    val config2: MonitorConfig = new MonitorConfig(
      "src/test/resources/file-move-test-dir",
      "E:\\dev\\github\\scala-pg\\akka\\src\\test\\resources\\file-copy-test-dir\\src",
      Move
    )
    config1 :: config2 :: Nil
  }

}

case class MonitorConfig(srcLocation: String, targetLocation: String, processingType: ProcessingType)


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
        li("copying file " + file.getFileName + " to " + targetLocation)
        copyFileToLocation(file, targetLocation)
      case MoveFile(file, targetLocation) =>
        li("moving file " + file.getFileName + " to " + targetLocation)
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

class FileProcessor(val monitorLoc: String, val targetLoc: String, val processingType: ProcessingType) {

  val monitorLocation = Paths.get(monitorLoc)
  val targetLocation = Paths.get(targetLoc)

  if (!Files.isDirectory(monitorLocation)) {
    throw new IllegalArgumentException("Location to monitor [" + monitorLocation + "]should be directory.")
  }
  if (!Files.isDirectory(targetLocation)) {
    throw new IllegalArgumentException("Target location [" + targetLocation + "]should be directory.")
  }

  def this(config: MonitorConfig) = this(config.srcLocation, config.targetLocation, config.processingType)


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
        li("Start monitoring location for new files: " + location + "\n" +
          "\ttarget (using " + processingType + ") is " + targetLocation)
        self ! Monitor()
    }
  }

}