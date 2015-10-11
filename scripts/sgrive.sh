#!/bin/sh
exec scala "$0" "$@"
!#
import sys.process._
import scala.util._
import scala.collection.mutable._
import java.io.{FileWriter, PrintWriter}
import java.util.Calendar
import java.text._

val buf = new ListBuffer[String]()
def log(msg: String) = {
  buf.append(msg)
  println(msg)
}


def using[A <: {def close(): Unit}, B](closeable: A)(f: A => B): B =
  try { f(closeable) } finally { closeable.close() }
  
def appendToFile(fileName:String, textData:String) =
  using (new FileWriter(fileName, true)){ 
    fileWriter => using (new PrintWriter(fileWriter)) {
      printWriter => printWriter.println(textData)
    }
  }  

def time[A](f: => A) = {
  val s = System.nanoTime
  val ret = f
  log("\tTime: "+(System.nanoTime-s)/1e9+"s")
  ret
}
 
def countFiles = ("find . -type f" #| "wc -l" !!).filter(_.isDigit).toInt
def execute(cmd: String) = {
  Try(cmd.!) match {
    case Success(retCode) => retCode
    case Failure(e)       => log("Failure: " + e.getMessage);999
  }
}

def run() = {
  val fileCountBefore = countFiles
  log(s"File count before: $fileCountBefore" )

  val cmds = Stream.fill(10)("grive") // 10 attempts
  val results = cmds.map{x => 
    log(s"\nExecuting: $x")
    val result = execute(x)
    log(s"Result: $result")
    result
  }.takeWhile(_ != 0)
  
  val failures = results.toList // force evalutation
  
  val fileCountAfter = countFiles 
  log(s"\nSummary:")
  log(s"\tFiles (before, after): ($fileCountBefore, $countFiles)")
  log(s"\tRetries: ${failures.size}")
  val outputFile = {
    val now = Calendar.getInstance().getTime()
    val sd = new SimpleDateFormat("yyMMdd:HHmmss")
    "sgrive_" + sd.format(now) + ".log" 
  }
  appendToFile(outputFile, buf./:("\n")(_ + "\n" + _))
}

time(run())
