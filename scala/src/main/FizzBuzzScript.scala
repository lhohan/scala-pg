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

numbersAndPattern take 100 foreach { println(_) }
