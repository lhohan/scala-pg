package scalaImpatient.ch12

import org.scalatest.FunSuite

class Ch12ExercisesTest extends FunSuite {

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


}
