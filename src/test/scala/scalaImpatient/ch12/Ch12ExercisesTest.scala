package scalaImpatient.ch12

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.{Span, Millis}
import org.scalatest.exceptions.TestFailedDueToTimeoutException

class Ch12ExercisesTest extends FunSuite with Timeouts {

  import scalaImpatient.ch12.Chapter12HigherOrderFunctions._

  test("exercise 1") {
    val result = values(x => x * x, -5, 5)
    assert((-5, 25) === result.head)
    assert((-4, 16) === result.tail.head)
    assert(11 === result.size)
    //    println(result)

    // some corner cases
    assert(values(x => x * x, 5, -5).isEmpty, "boundary reverse test")

  }

  test("exercise 2") {
    assert(4 === maxUsingReduceLeft(Array(1, 4, 2, 0, -6)))
    assert(1 === maxUsingReduceLeft(Array(1)))
    intercept[IllegalArgumentException] {
      maxUsingReduceLeft(Array())
    }
  }


  test("exercise 3") {
    assert(6 === factorialUsingReduceLeft(3))
    assert(120 === factorialUsingReduceLeft(5))
    assert(1 === factorialUsingReduceLeft(1))
    assert(1 === factorialUsingReduceLeft(0))
    intercept[IllegalArgumentException] {
      factorialUsingReduceLeft(-1)
    }

  }

  test("exercise 4") {
    assert(6 === factorialUsingFoldLeft(3))
    assert(120 === factorialUsingFoldLeft(5))
    assert(1 === factorialUsingFoldLeft(1))
    assert(1 === factorialUsingFoldLeft(0))
    intercept[IllegalArgumentException] {
      factorialUsingFoldLeft(-1)
    }

  }

  test("exercise 5") {
    assert(25 === largest(x => 10 * x - x * x, 1 to 10))
    assert(10 === largest(x => x, 1 to 10))
  }

  test("exercise 6") {
    assert(5 === largestAt(x => 10 * x - x * x, 1 to 10))
    assert(10 === largestAt(x => x, 1 to 10))
    assert(1 === largestAt(x => -x, 1 to 10))
  }

  test("exercise 7") {
    val pairs = (1 to 10) zip (11 to 20)
    val s: Seq[Int] = sum(pairs)
    assert(12 === s.head)
    assert(30 === s(9))
  }

  test("exercise 8") {
    assert(compareStringLengthsToValue(Array("abcdef"), Array(6)), "exercise 8.1.1")
    assert(!compareStringLengthsToValue(Array("abcdef"), Array(7)), "exercise 8.1.2")
    assert(compareStringLengthsToValue(Array("hello", "world", "!"), Array(5, 5, 1)), "exercise 8.2")
    assert(compareStringLengthsToValue(Array(), Array()), "exercise 8.3")
    assert(!compareStringLengthsToValue(Array("hello", "world", "!"), Array(4, 5, 1)), "exercise 8.4")
  }

  test("exercise 10") {
    var x = 1
    unless(x > 5) {
      x += 1
    }
    assert(6 === x)
  }

  test("exercise 10 - no call by name") {
    var x = 1
    intercept[TestFailedDueToTimeoutException] {
      failAfter(Span(200, Millis))(
        unlessNoCallByName(x > 5) {
          x += 1
          Thread.sleep(10) // without this failAfter does not fail
        }
      )
    }
    assert(x > 6) // x will keep on counting past 6 (note this test may fail of failAfter timeout is not high enough)
  }

  test("exercise 10 - no currying") {
    var x = 1
    unlessNoCurrying(x > 5, {
      x += 1
    })
    assert(6 === x)
  }


}
