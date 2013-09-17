package misc

import sun.beans.editors.DoubleEditor

/**
 * User: hanlho
 * DateTime: 17/09/13 21:29
 */
object NewtonRaphson {

  def abs(x:Double) = if(x<0) -x else x


  def sqrt(x: Double): Double = {

    def isGoodEnough(guess: Double, tolerance:Double): Boolean = abs(guess*guess -x) < tolerance

    def improve(x1: Double, guess: Double): Double = (guess + x1 / guess) / 2

    def sqrtIter(guess: Double, tolerance: Double): Double =
      if (isGoodEnough(guess, tolerance)) guess
      else
        sqrtIter(improve(x, guess), tolerance)

    sqrtIter(1, 0.0001)
  }

}
