package misc

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers

class FizzBuzzTest extends FunSuite with Checkers {

  import misc.FizzBuzz._

  test("fizzbuzz") {
    val fb = fizzbuzz(100)
    val range = Gen.choose[Int](1, 100)
    val fizzbuzzProperty = forAll(range) {
      n =>
        if (n % 15 == 0) fb(n - 1) == "FizzBuzz" else
        if (n % 5 == 0) fb(n - 1) == "Buzz" else
        if (n % 3 == 0) fb(n - 1) == "Fizz" else
          fb(n - 1) == n.toString

    }
    check(fizzbuzzProperty)
  }
}