package funprog.ch02

/**
 * User: hanlho
 * DateTime: 20/05/13 9:09
 */
object Ch02GettingStarted {


  // exercise 1
  case class Box(height: Int, width: Int)

  def taller(b1: Box, b2: Box): Box = greatherBy(b1, b2, _.height)

  def wider(b1: Box, b2: Box): Box = greatherBy(b1, b2, _.width)


  def greatherBy(b1: Box, b2: Box, f: Box => Int): Box = {
    if (f(b1) > f(b2)) b1 else b2
  }

  // exercise 2
  def abs(x: Int) = if (x < 0) -x else x

  def neg(x:Int) = -x // test function
  def timesTwo(x:Int) = 2 * x // test function

  def absolute(f: Int => Int): Int => Int = {
    x => abs(f(x))
  }

}
