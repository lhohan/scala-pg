package scalaImpatient.ch05

object Chapter05Classes {

  // exercise 1
  class Counter {

    private var value = 0

    def increment() {
      if (value < Int.MaxValue) {
        value += 1
      }
    }

    def current = value

  }

}
