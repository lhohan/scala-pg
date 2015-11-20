package misc

object FizzBuzz {
  //  def fizzbuzz(s: Int): List[String] = {
  //
  //    def shout(i: Int): String =
  //      if (i % 15 == 0) "FizzBuzz" else
  //      if (i % 3 == 0) "Fizz" else
  //      if (i % 5 == 0) "Buzz" else
  //        i.toString
  //
  //    val fb: Stream[String] = Stream.from(1).map(i => shout(i))
  //
  //    (fb take s).toList
  //  }

  def fizzbuzz(s: Int): List[String] = {

    // helper method inspired by haskell, cycle a list infinitely,
    def cycle(xs: List[String]): Stream[String] = Stream.continually(xs).flatten

    val numbers = Stream.from(1)

    // a infinite cycle of "", "", "Fizz"
    val fizzes = cycle(List("", "", "Fizz"))
    // a infinite cycle of "", "", "", "", "Buzz"
    val buzzes = cycle(List("", "", "", "", "Buzz"))

    // zip the fizzes and buzzes, and concatenate them, result is "", "", "Fizz", "", "Buzz", "Fizz", ...
    val pattern = fizzes zip buzzes map { case (f, b) => f + b }
    // zip numbers with the pattern, if the pattern is empty keep the number, otherwise keep the pattern
    val numbersAndPattern = numbers zip pattern map {
      case (n, p) => if (p.isEmpty) n.toString else p
    }

    numbersAndPattern take s toList
  }

}
