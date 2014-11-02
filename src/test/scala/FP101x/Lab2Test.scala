package FP101x

import org.scalatest.FunSuite

/**
 * Created by hans on 02/11/14.
 */
class Lab2Test extends FunSuite{

  import Lab2._

  test("toDigits"){

    List(4485429517622493L, 4320635998241421L, 4929778869082405L, 5256283618614517L).foreach
    { x =>
        assert(eval(toDigits(x)) == x, s"test of $x")
    }
  }

}
