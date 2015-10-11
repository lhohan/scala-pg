package scalaImpatient.ch06

import org.scalatest.FunSuite
import scalaImpatient.ch06.Chapter06Objects._

class Ch06ExercisesTest extends FunSuite {

  test("exercise 1") {
    val centimeters = Conversions.inchesToCentimeters(2.54)
    assert(1 === centimeters)
    val km = Conversions.milesToKilometers(1.609344)
    assert(1 === km)
    val liters = Conversions.gallonsToLiters(3.78541178)
    assert(1 === liters)
  }

  test("exercise 2") {
    val centimeters = InchesToCentimeters.convert(2.54)
    assert(1 === centimeters)
    val km = MilesToKilometers.convert(1.609344)
    assert(1 === km)
    val liters = GallonsToLiters.convert(3.78541178)
    assert(1 === liters)
  }

  test("exercise 3") {
    assert(0 === Origin.getX)
    assert(0 === Origin.getY)
  }

  test("exercise 4: point creating using apply method") {
    val p = Point(3, 4)
    assert(3 === p.x)
    assert(4 === p.y)
  }

  test("exercise 6 - playing cards enum") {
    //    PlayingCard.values.foreach(println(_))
    assert("♠" === PlayingCard.♠.toString)
    assert("♦" === PlayingCard.♦.toString)
    assert("♥" === PlayingCard.♥.toString)
    assert("♠" === PlayingCard.♠.toString)
  }

  test("exercise 7 - is red test") {
    assert(!PlayingCard.isRed(PlayingCard.♠))
    assert(PlayingCard.isRed(PlayingCard.♦))
    assert(PlayingCard.isRed(PlayingCard.♥))
    assert(!PlayingCard.isRed(PlayingCard.♠))
  }

  test("exercise 8") {
    assert(0xff0000 === RGBColorCuberCorner.RED.id)
  }

}
