package misc

import org.scalatest.FunSuite

/**
 * User: hanlho
 * DateTime: 31/03/13 20:09
*/
class FibonacciTest extends FunSuite{

  import misc.Fibonacci.generateFibonacci

  test("test Finbonacci sequence of 2") {
    def fib = generateFibonacci(2)
    assert("1,1" === fib.mkString(","))
  }

  test("test Finbonacci sequence of 1") {
    def fib = generateFibonacci(1)
    assert("1" === fib.mkString(","))
  }

  test("test Finbonacci sequence of 3") {
    def fib = generateFibonacci(3)
    assert("1,1,2" === fib.mkString(","))
  }

  test("test Finbonacci sequence of 7") {
    def fib = generateFibonacci(7)
    assert(3 === fib(3))
    assert(5 === fib(4))
    assert(8 === fib(5))
    assert(13 === fib(6))
  }

}
