package scalaImpatient.ch12

object Chapter12HigherOrderFunctions {

  //exercise 1
  def values(fun: (Int) => Int, low: Int, high: Int) = {
    def valuesIter(fun: (Int) => Int, input: Int):List[Tuple2[Int,Int]] = {
      if (input > high) Nil
      else (input, fun(input)) +: valuesIter(fun, input +1)
    }
    valuesIter(fun, low)
  }
}
