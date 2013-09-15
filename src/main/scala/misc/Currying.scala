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

  def process[A](filter: A => Boolean)(list: List[A]): List[A] = {
    lazy val recurse = process(filter) _

    list match {
      case head :: tail => if (filter(head)) {
        head :: recurse(tail)
      } else {
        recurse(tail)
      }
      case Nil => Nil
    }
  }

  val even = (a: Int) => a % 2 == 0

  val numberAsc = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
  val numberDesc = 5 :: 4 :: 3 :: 2 :: 1 :: Nil

  println(process(even)(numberAsc))
  println(process(even)(numberDesc))

  val processEvens = process(even) _ // '_' means :'treat as functional value rather than a method'
  println("processEvens(numberAsc) = " + processEvens(numberAsc))
  println("processEvens(numberDesc) = " + processEvens(numberDesc))

  //  Partials without Currying
  def add(x: Int, y: Int, z: Int) = x + y + z

  val addFive = add(5, _: Int, _: Int)
  println("addFive to 3 and 4 = " + addFive(3, 4))

  val addFive2 = (x: Int, y: Int) => add(5, x, y)
  println("addFive2 to 3 and 4 = " + addFive2(3, 4))


  //  ex5
  //  ex4
  //  ex3
  //  ex2
  //  ex1


}
