package funprog.ch02

import org.scalatest.FunSuite

/**
 * User: hanlho
 * DateTime: 20/05/13 9:11
 */
class Ch02ExercisesTest extends FunSuite {

  import Ch02GettingStarted._

  test("exercise 1") {
    val box1 = new Box(3, 8)
    val box2 = new Box(5, 4)

    assert(box2 === taller(box1, box2))
    assert(box1 === wider(box1, box2))

  }

  test("exercise 2") {
    assert(5 === abs(5))
    assert(5 === abs(-5))

    assert(-5 === neg(5))
    assert(10 === timesTwo(5))

    assert(5 === absolute(neg)(5))
    val absoluteTimesTwo = absolute(timesTwo)
    assert(12 === absoluteTimesTwo(-6))

  }





}
