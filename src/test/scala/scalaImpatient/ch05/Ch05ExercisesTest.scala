package scalaImpatient.ch05

import org.scalatest.FunSuite

class Ch05ExercisesTest extends FunSuite {

  import scalaImpatient.ch05.Chapter05Classes._

  // exercise 1
  test("test value Counter does not get incremented to negative value") {
    val c: Counter = new Counter
    // quick implementation to increment to Max value
    while (c.current != Int.MaxValue) c.increment()
    assert(Int.MaxValue === c.current)
    // increment when MaxValue
    c.increment()
    assert(Int.MaxValue === c.current)
  }

  //exercise 2
  test("test BankAccount class") {
    val ba: BankAccount = new BankAccount
    assert(0 === ba.balance)
    ba.deposit(100)
    assert(100 === ba.balance)
    ba.withdraw(20)
    assert(80 === ba.balance)
  }

  // exercise 3
  test("test Time class") {
    val t1 = new Time(0, 30)
    val t2 = new Time(0, 45)
    val t3 = new Time(0, 15)
    val t4 = new Time(23, 15)

    assert(t1.before(t2), "time test 1")
    assert(!t1.before(t3), "time test 2")
    assert(t1.before(t4), "time test 3")
  }

  // exercise 4
  test("test Time2 class using same tests as exercise 3") {
    val t1 = new Time2(0, 30)
    val t2 = new Time2(0, 45)
    val t3 = new Time2(0, 15)
    val t4 = new Time2(23, 15)

    assert(t1.before(t2), "time test 1")
    assert(!t1.before(t3), "time test 2")
    assert(t1.before(t4), "time test 3")
  }

}
