package scalaImpatient.ch05

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

}
