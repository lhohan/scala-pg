package anki

import org.scalatest.FlatSpec

/**
 * User: hanlho
 * DateTime: 3/04/2014 7:39
 */
class AnkiTest extends FlatSpec {
  import anki.Anki._

  "grouping lines" should "split a list of lines in a list of list of lines based on empty lines" in {

    val lines = List("line 1", "line 2", "   ", "", "", "lines 3", "line 4", "")

    val grouped = group(lines)

    assert(2 === grouped.size, s"wrong number of groupings in $grouped")
  }

  "creating a new deck on setup where first line contains front and second contains back for 3 cards definitions" should
    "return a deck of 3 cards" in {

    val lines = List("front 1","back 1", "  ", "front 2","  back 2", "","   front 3","back 3")

    val deck = newDeck(lines)

    assert(3 === deck.size, s"wrong number of cards in $deck")
  }
}
