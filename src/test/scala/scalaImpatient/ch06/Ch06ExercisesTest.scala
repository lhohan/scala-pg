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
}
