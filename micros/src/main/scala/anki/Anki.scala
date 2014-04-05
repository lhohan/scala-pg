package anki

import scala.annotation.tailrec
import scala.io.Source

object AnkiApp extends App {

  import Anki._

  val inputFile = {
    if (args.length > 0) args(0)
    else "./anki.txt"
  }

  val outFile = {
    if (args.length > 1) args(1)
    else "./ankiImport.txt"
  }

  val lines = Source.fromFile(inputFile).getLines.filterNot(_.trim.startsWith("#")).toList
  //lines.foreach(println(_))

  val deck = newDeck(lines)
  deck.foreach(println(_))

}

object Anki {

  case class Card(front: String, back: String)

  type Deck = List[Card]

//  TODO @tailrec
  def group(lines: List[String]): List[List[String]] = {
    if (lines.isEmpty) List(List())
    else {
      val (current, next) = lines.span(!_.trim.isEmpty)
      val nextNoEmptyHead = next.dropWhile(_.trim.isEmpty)
      if (nextNoEmptyHead == Nil) List(current)
      else current :: group(nextNoEmptyHead)
    }
  }

  def newDeck(lines: List[String]): Deck = {
    val (valid, invalid) = group(lines).partition(_.size == 2)
    invalid.foreach(x => println(s"Invalid input. Could not get card from ${x}"))
    valid.map(list => Card(list(0),list(1)))
  }

}
