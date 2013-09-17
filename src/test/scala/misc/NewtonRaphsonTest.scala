package misc

import org.scalatest.FunSuite

/**
 * User: hanlho
 * DateTime: 17/09/13 21:31
 */
class NewtonRaphsonTest extends FunSuite {

  import misc.NewtonRaphson._

  test("test Newton-Raphson method sqrt of 2") {
    expectResult(true) {
       abs(sqrt(2) - 1.4142) < 0.001
    }
  }

}
