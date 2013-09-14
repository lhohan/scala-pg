package misc

/**
 * User: hanlho
 * DateTime: 14/09/13 10:29
 *
 * http://www.codecommit.com/blog/scala/function-currying-in-scala
 */

object Currying extends App {

  def ex1 = {

    def add(x: Int, y: Int) = x + y

    println(add(1, 7))

  }

  def ex2 = {

    def add(x: Int) = (y: Int) => x + y

    println(add(1)(7))

  }

  def ex3 = {

    def add(x: Int)(y: Int) = x + y

    println(add(1)(7))

  }

  def ex4 = {

    def add(x: Int, y: Int) = x + y
    val addCurried = Function.curried(add _)

    println(add(1, 7))
    println(addCurried(1)(7))

  }

  def ex5 = {

    def add(x: Int)(y: Int) = x + y
    val addUnCurried = Function.uncurried(add _)

    println(add(1)(7))
    println(addUnCurried(1, 7))

  }

  ex5
  ex4
  ex3
  ex2
  ex1


}
