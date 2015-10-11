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
     **/
  }

  // exercise 6
  test("Person class, negative values for age are set to 0") {
    val p = new Person
    p.age = 25
    assert(25 === p.age)

    p.age = -10
    assert(0 === p.age)
  }

  // exercise 7
  test("Person2 class, first and last name from name") {
    val p = new Person2("Jeff Smith")
    assert("Jeff" === p.firstName)
    assert("Smith" === p.lastName)
    //    does not work:
    //    assert("Jeff Smith" === p.name)
  }

  // exercise 8
  test("Car class tests") {
    val c = new Car("BMW", "320d")
    assert("BMW" === c.manufacturer)
    assert("320d" === c.modelName)
    assert("320d" === c.modelName)
    assert(-1 === c.modelYear)
    assert("" === c.licensePlate)
    c.licensePlate = "1-CUR-314"
    assert("1-CUR-314" === c.licensePlate)
    //  does not work
    //  c.modelName = "Bugatti"

    val c1 = new Car("BMW", "320d", modelYear = 2012)
    assert("BMW" === c1.manufacturer)
    assert("320d" === c1.modelName)
    assert(2012 === c1.modelYear)
    assert("" === c1.licensePlate)

    val c2 = new Car("BMW", "320d", licensePlate = "1-CUR-314")
    assert("BMW" === c2.manufacturer)
    assert("320d" === c2.modelName)
    assert(-1 === c2.modelYear)
    assert("1-CUR-314" === c2.licensePlate)

    val c3 = new Car("BMW", "320d", licensePlate ="ABC-123", modelYear = 2012)
    assert("BMW" === c3.manufacturer)
    assert("320d" === c3.modelName)
    assert(2012 === c3.modelYear)
    assert("ABC-123" === c3.licensePlate)

  }

  // exercise 9 : implement the above in Java: I get the point: I'll be writing a whole lot more code ;-)

  test("employee class"){
    val e1 = new Employee()
    assert("John Q. Public" === e1.name)
    assert(0.0 === e1.salary)

    val e2 = new Employee("Jack Bauer", 100000)
    assert("Jack Bauer" === e2.name)
    assert(100000 === e2.salary)


    val e3 = new Employee2()
    assert("John Q. Public" === e3.name)
    assert(0.0 === e3.salary)

    val e4 = new Employee2("Jack Bauer", 100000)
    assert("Jack Bauer" === e4.name)
    assert(100000 === e4.salary)

  }

}
