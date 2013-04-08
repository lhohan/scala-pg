package scalaImpatient.ch06

import java.awt.Point

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
  object Origin extends Point(0,0){

  }


}
