package scalaImpatient.ch10

import org.scalatest.FunSuite
import java.awt.geom.Ellipse2D
import scalaImpatient.ch10.Chapter10Traits._
import java.awt.Point
import java.beans.{PropertyChangeSupport, PropertyChangeEvent, PropertyChangeListener}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import java.io.{ByteArrayInputStream, InputStream}
import scala.collection.mutable.ArrayBuffer

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

  test("exercise 2") {
    assert(new OrderPoint(2, 0).compare(new OrderPoint(1, 2)) > 0, "check 1")
    assert(new OrderPoint(1, 2).compare(new OrderPoint(1, 2)) == 0, "check 3")
    assert(new OrderPoint(1, 1).compare(new OrderPoint(1, 2)) < 0, "check 2")
    assert(new OrderPoint(0, 0).compare(new OrderPoint(1, 2)) < 0, "check 4")
  }

  test("exercise 4 - default") {
    val logger = new CryptoLogger() // default 3
    assert("d" === logger.encrypt("a"))
    assert("}" === logger.encrypt("z"))
    assert("Fdhvdu" === logger.encrypt("Caesar"))
  }

  test("exercise 4 - with cypher -3") {
    val logger = new CryptoLogger(-3) // default 3
    assert("a" === logger.encrypt("d"))
    assert("z" === logger.encrypt("}"))
    assert("Caesar" === logger.encrypt("Fdhvdu"))
  }

  test("exercise 5") {
    val p = new Point with PropertyChangeSupportLike {
      // not sure if this how propertychangesupport is supposed to be triggered
      // (expected something more automatic)
      override def setLocation(x1: Int, y1: Int) {
        firePropertyChange("location", x + "-" + y, x1 + "-" + y1)
        x = x1
        y = y1
      }
    }
    // add a listener for testing purposes to just set a flag when a property changed
    var detected = false
    p.addPropertyChangeListener(new PropertyChangeListener {
      def propertyChange(evt: PropertyChangeEvent) {
        detected = true
        //        println("change field " + evt.getPropertyName + " to " + evt.getNewValue)
      }
    })
    assert(!detected)

    p.setLocation(5, 7)
    assert(detected)
  }

  test("exercise 8") {
    def readAll(s: String): String = {
      val in: InputStream = new ByteArrayInputStream(s.getBytes("UTF-8")) with Buffered
      var stuffRead = new ArrayBuffer[Char]()
      var byte = in.read()
      while (byte != -1) {
        stuffRead += byte.toChar
        byte = in.read()
      }
      stuffRead.mkString
    }

    assert("" === readAll(""))
    assert("A" === readAll("A"))
    // text size of default buffer size
    assert("12345678" === readAll("12345678"))
    // text size one longer than buffer size
    assert("123456789" === readAll("123456789"))
    // text size one shorter than buffer size
    assert("1234567" === readAll("1234567"))
    // text size somewhat longer
    assert("Some text longer than default buffer size." === readAll("Some text longer than default buffer size."))

  }

  test("exercise 9") {
    def readAll(s: String): String = {
      val in = new ByteArrayInputStream(s.getBytes("UTF-8")) with BufferedWithLogger
      val myBuffer = new ArrayBuffer[String]() // this buffer we use to capture everything that is logged
      in.logger = new ArrayBufferLogger(myBuffer)
      var byte = in.read
      while (byte != -1) {
        byte = in.read
      }
      myBuffer.mkString
    }

    // same tests as ex. 8
    assert("" === readAll(""))
    assert("A" === readAll("A"))
    // text size of default buffer size
    assert("12345678" === readAll("12345678"))
    // text size one longer than buffer size
    assert("123456789" === readAll("123456789"))
    // text size one shorter than buffer size
    assert("1234567" === readAll("1234567"))
    // text size somewhat longer
    assert("Some text longer than default buffer size." === readAll("Some text longer than default buffer size."))

  }


//  test("exercise 10") {
//    val in = new ByteArrayInputStream("hello iterable stream".getBytes("UTF-8")) with IterableInputStream
//    for (i <- in){
//      println(i)
//    }
//  }
}