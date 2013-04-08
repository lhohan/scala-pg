package scalaImpatient.ch06

// exercise 5
// run in sbt:
//              run scalaImpatient.ch06.Reverse Hello World
object Reverse extends App {
  if (args.nonEmpty) {
      println(args.tail.reverse.mkString(" "))
  }
}
