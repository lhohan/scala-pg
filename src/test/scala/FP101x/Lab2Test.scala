package FP101x

import org.scalatest.FunSuite

/**
 * Created by hans on 02/11/14.
 */
class Lab2Test extends FunSuite {

  import FP101x.Lab2._

  test("toDigits") {
    List(4485429517622493L, 4320635998241421L, 4929778869082405L, 5256283618614517L).foreach { x =>
      assert(eval(toDigits(x)) == x, s"test of $x")
    }
  }

  test("toDigitsRev") {
    List(4485429517622493L, 4320635998241421L, 4929778869082405L, 5256283618614517L).foreach { x =>
      assert(evalRev(toDigitsRev(x)) == x, s"test of $x")
    }
  }

  test("doubleSecond") {
    assertResult(List(8, 14, 6, 10), "example")(doubleSecond(List(8, 7, 6, 5)))
  }

  test("sumDigits") {
    assertResult(20, "case 1")(sumDigits(List(8, 14, 6, 10)))
    assertResult(30, "case 2")(sumDigits(List(3, 9, 4, 15, 8)))
  }

  test("isValid") {
    assert(isValid(4012888888881881L), "4012888888881881")
    assert(!isValid(4012888888881891L), "4012888888881891")
  }

}
