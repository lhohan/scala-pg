package scalaImpatient.ch10

import org.scalatest.FunSuite
import java.awt.geom.Ellipse2D
import scalaImpatient.ch10.Chapter10Traits._

class Ch10ExercisesTest extends FunSuite {

  test("exercise 1") {
    val egg = new Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
    egg.translate(10, -10)
    egg.grow(100, 200)
    assert(15 === egg.getX)
    assert(0 === egg.getY)
    assert(120 === egg.getWidth)
    assert(230 === egg.getHeight)
  }

  test("exercise 2: same order") {
    assert(new OrderPoint(2, 0).compare(new OrderPoint(1, 2)) > 0, "check 1")
    assert(new OrderPoint(1,2).compare(new OrderPoint(1, 2)) == 0, "check 3")
    assert(new OrderPoint(1, 1).compare(new OrderPoint(1, 2)) < 0, "check 2")
    assert(new OrderPoint(0, 0).compare(new OrderPoint(1, 2)) < 0, "check 4")
  }

}
