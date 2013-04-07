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

  // exercise 5
  test("test Student class Java bean properties") {
    val s = new Student("Chuck", 1)
    assert("Chuck" === s.name)
    assert("Chuck" === s.getName)
    assert(1 === s.id)
    assert(1 === s.getId)
    // test setting value using Scala property setter
    s.name = "Norris"
    s.id = 2
    assert("Norris" === s.name)
    assert("Norris" === s.getName)
    assert(2 === s.id)
    assert(2 === s.getId)
    // test setting value using Java property setter
    s.setName("Bruce")
    s.setId(3)
    assert("Bruce" === s.name)
    assert("Bruce" === s.getName)
    assert(3 === s.id)
    assert(3 === s.getId)

    /**
     * Result javap:
     * PS ...\scala-pg> &'C:\Program Files\Java\jdk1.7.0_05\bin\javap' '.\target\scala-2.9.2\classes\scalaImpatient\c
h05\Chapter05Classes$Student.class'
Compiled from "Chapter05Classes.scala"

public class scalaImpatient.ch05.Chapter05Classes$Student implements scala.ScalaObject {
  public java.lang.String name();
  public void name_$eq(java.lang.String);
  public void setName(java.lang.String);
  public long id();
  public void id_$eq(long);
  public void setId(long);
  public long getId();
  public java.lang.String getName();
  public scalaImpatient.ch05.Chapter05Classes$Student(java.lang.String, long);
}
     *
     * */
  }

}
