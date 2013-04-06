package scalaImpatient.ch05

import org.scalatest.FunSuite

class Ch05ExercisesTest extends FunSuite{

  import scalaImpatient.ch05.Chapter05Classes._

  // exercise 1
  test("test value Counter does not get incremented to negative value"){
     val c : Counter = new Counter
    // quick implementation to increment to Max value
    while(c.current != Int.MaxValue) c.increment()
    assert(Int.MaxValue === c.current)
    // increment when MaxValue
    c.increment()
    assert(Int.MaxValue === c.current)
  }

}
