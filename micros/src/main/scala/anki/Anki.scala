package anki

import scala.io.Source
import java.io.PrintWriter

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

  val linesInMem = Source.fromFile(inputFile).getLines.toList
  val deck = newDeck(linesInMem)
  val writer = new PrintWriter(outFile)
  val validCards  = deck.filter(_.valid)
  validCards.foreach(card => writer.println(card.front + "\t" + card.back + "\t" + card.detail + "\t" + card.info))
  writer.close()

  //printSummary
  println(s"Cards written: ${validCards.size}.")
  if(deck.size > validCards.size){
    deck.filterNot(_.valid).foreach(c => println("Invald card found. Skipped: " + c))
  }
}

object Anki {

  case class Card(front: String, back: String, detail: String = "", info: String = "", valid: Boolean = true)

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
    val cardLines = group(lines)
    cardLines.map {
      list =>
        val detail = list.filter(_.startsWith(".")).map(_.tail).mkString(" ")
        val info = list.filter(_.startsWith("#")).map(_.tail).mkString(" ")
        val frontAndBackLines = list.filterNot(line => line.startsWith(".") || line.startsWith("#"))
        frontAndBackLines match {
          case Nil => Card("No front content", "No back content", detail, info, false)
          case front :: Nil => Card(front, "No back content", detail, info, false)
          case front :: rest => Card(front, rest.mkString(" "), detail, info)
        }
    }
  }

}
