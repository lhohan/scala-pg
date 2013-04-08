package scalaImpatient.ch05

import scala.reflect.BeanProperty

object Chapter05Classes {

  // exercise 1
  class Counter {

    private var value = Int.MaxValue - 100 // for testing purposes just set it a bit close to MaxValue

    def increment() {
      if (value < Int.MaxValue) {
        value += 1
      }
    }

    def current = value

  }

  //exercise 2
  class BankAccount {

    private var privateBalance = 0

    def deposit(amount: Int) {
      privateBalance += amount
    }

    def withdraw(amount: Int) {
      privateBalance -= amount
    }

    def balance = this.privateBalance
  }

  //exercise 3
  class Time(val hrs: Int, val min: Int) {
    def before(other: Time): Boolean = {
      if (hrs < other.hrs) true
      else if (hrs == other.hrs && min < other.min) true
      else false
    }
  }

  //exercise 4
  class Time2(hrs: Int, min: Int) {
    val totMin = hrs * 60 + min

    def before(other: Time2): Boolean = totMin < other.totMin
  }

  // exercise 5
  class Student(@BeanProperty var name: String, @BeanProperty var id: Long)

  // exercise 6
  class Person {
    private var privateAge = 0

    def age = privateAge

    def age_=(newAge: Int) {
      if (newAge < 0) privateAge = 0
      else privateAge = newAge
    }
  }

  // exercise 7
  class Person2(name: String) {
    def firstName = name.split(" ")(0)

    def lastName = name.split(" ")(1)

    // needs error and corner case handling

    // on the 'why?' question: there can be different reasons why you would chose val, var or as I did: nothing
    // I chose this implementation as it's the easiest one that worked
  }

  // exercise 8
  class Car(val manufacturer: String, val modelName: String, val modelYear: Int = -1, var licensePlate: String = "")

  // exercise 10 - original class
  class Employee(val name: String, var salary: Double) {
    def this() {
      this("John Q. Public", 0.0)
    }
  }

  // exercise 10 - solution class
  class Employee2() {
    private var _name: String = "John Q. Public"
    var _salary: Double = 0.0

    def this(name : String, salary : Int) {
      this()
      _name = name
      _salary = salary
    }

    def name  = {
      _name
    }

    def salary = {
      _salary
    }
  }

}
