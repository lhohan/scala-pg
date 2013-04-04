package scalaImpatient.ch12

object Chapter12HigherOrderFunctions {

  //exercise 1
  def values(fun: (Int) => Int, low: Int, high: Int) = {
    def valuesIter(fun: (Int) => Int, input: Int): List[Tuple2[Int, Int]] = {
      if (input > high) Nil
      else (input, fun(input)) +: valuesIter(fun, input + 1)
    }
    valuesIter(fun, low)
  }

  // exercise 2
  def maxUsingReduceLeft(arr: Array[Int]): Int = {
    require(arr.size > 0)
    arr.reduceLeft((x: Int, y: Int) => if (x > y) x else y)
  }

  // exercise 3
  def factorialUsingReduceLeft(n: Int): Int = {
    require(n > -1)
    if (n == 0) 1
    else
      (1 to n).reduceLeft(_ * _)
  }

  // exercise 4
  def factorialUsingFoldLeft(n: Int): Int = {
    require(n > -1)
    (1 to n).foldLeft(1)(_ * _)
  }

  // exercise 5
  def largest(fun: (Int) => (Int), inputs: Seq[Int]): Int = {
    inputs.map(fun(_)).reduceLeft(((largest: Int, x: Int) => if (largest > x) largest else x))
  }

  // exercise 6
  def largestAt(fun: (Int) => (Int), inputs: Seq[Int]): Int = {
    inputs.foldLeft(inputs.head)((max: Int, input: Int) => if (fun(max) > fun(input)) max else input)
  }

  // exercise 7
  def sum(pairs: Seq[Tuple2[Int, Int]]): Seq[Int] = {
    def adjustToPair(function: (Int, Int) => Int)(pair: Tuple2[Int, Int]) = function(pair._1, pair._2)
    pairs.map((adjustToPair(_ + _)))
  }


}
