package scalaImpatient.ch06

import java.awt.{Color, Point}
import java.awt

object Chapter06Objects {

  // exercise 1
  object Conversions {

    def gallonsToLiters(gallons: Double) = {
      gallons / 3.78541178
    }

    def milesToKilometers(miles: Double) = {
      miles / 1.609344
    }

    def inchesToCentimeters(inches: Double) = {
      inches / 2.54
    }
  }

  // exercise 2
  class UnitConversion(unit: Double) {
    def convert(value: Double) = value / unit
  }

  object InchesToCentimeters extends UnitConversion(2.54)

  object GallonsToLiters extends UnitConversion(3.78541178)

  object MilesToKilometers extends UnitConversion(1.609344)

  // exercise 3
  object Origin extends awt.Point(0, 0) {}

  // exercise 4
  class Point(val x: Double, val y: Double)

  object Point {
    def apply(x: Double, y: Double) = new Point(x, y)
  }

  // exercise 6
  object PlayingCard extends Enumeration {
    type PlayingCard = Value
    val ♣ = Value("\u2663")
    val ♦ = Value("\u2666")
    val ♥ = Value("\u2665")
    val ♠ = Value("\u2660")

    // exercise 7
    def isRed(p: PlayingCard): Boolean = ((p == ♦) || (p == ♥))

  }

  // exercise 8
  object RGBColorCuberCorner extends Enumeration {
    val RED = Value(0xff0000, "RED")
    // ...
  }

}
