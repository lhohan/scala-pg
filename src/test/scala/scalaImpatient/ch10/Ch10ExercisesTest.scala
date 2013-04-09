package scalaImpatient.ch10

import org.scalatest.FunSuite
import java.awt.geom.Ellipse2D
import scalaImpatient.ch10.Chapter10Traits._

class Ch10ExercisesTest extends FunSuite {

  test("exercise 1"){
    val egg = new Ellipse2D.Double(5,10,20,30) with RectangleLike
    egg.translate(10,-10)
    assert(true)
  }

}
