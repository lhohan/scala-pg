package scalaImpatient.ch10

import java.awt.geom.Ellipse2D
import java.awt.Point
import java.beans.{PropertyChangeEvent, PropertyChangeListener, PropertyChangeSupport}
import java.beans

object Chapter10Traits {

  // exercise 1
  trait RectangleLike extends Ellipse2D.Double {

    def translate(dx: Double, dy: Double) {
      this.x += dx
      this.y += dy
    }

    def grow(dwith: Double, dheight: Double) {
      this.width += dwith
      this.height += dheight
    }

  }

  // exercise 2
  class OrderPoint(x: Int, y: Int) extends java.awt.Point(x, y) with Ordered[Point] {

    def compare(that: Point) = {
      if ((x < that.getX || x == that.getX) && y < that.getY)
        -1
      else if (x == that.getX && y == that.getY)
        0
      else
        1
    }

  }

  // exercise 4
  trait Logger {
    def log(msg: String)
  }

  class CryptoLogger(var cypher: Int = 3) extends Logger {

    def encrypt(s: String): String = {
      s.toCharArray.map {
        c =>
          val cyphered: Int = c.toInt + cypher
          cyphered.toChar
      }.mkString
    }

    def log(msg: String) {
      println(encrypt(msg))
    }

  }

  //exercise 5
  trait PropertyChangeSupportLike {
    private val support = new PropertyChangeSupport(this)

    def addPropertyChangeListener(listener: PropertyChangeListener) {
      support.addPropertyChangeListener(listener)
    }

    def removePropertyChangeListener(listener: PropertyChangeListener) {
      support.removePropertyChangeListener(listener)
    }

    def getPropertyChangeListeners = support.getPropertyChangeListeners

    def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener) {
      support.addPropertyChangeListener(propertyName, listener)
    }

    def removePropertyChangeListener(propertyName: String, listener: PropertyChangeListener) {
      support.removePropertyChangeListener(propertyName, listener)
    }

    def getPropertyChangeListeners(propertyName: String) = support.getPropertyChangeListeners

    def firePropertyChange(propertyName: String, oldValue: Any, newValue: Any) {
      support.firePropertyChange(propertyName, oldValue, newValue)
    }

    def firePropertyChange(propertyName: String, oldValue: Int, newValue: Int) {
      support.firePropertyChange(propertyName, oldValue, newValue)
    }

    def firePropertyChange(propertyName: String, oldValue: Boolean, newValue: Boolean) {
      support.firePropertyChange(propertyName, oldValue, newValue)
    }

    def firePropertyChange(event: PropertyChangeEvent) {
      support.firePropertyChange(event)
    }

    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Any, newValue: Any) {
      support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)
    }

    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Int, newValue: Int) {
      support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)
    }

    def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Boolean, newValue: Boolean) {
      support.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)
    }

    def hasListeners(propertyName: String) = support.hasListeners(propertyName)
  }


}
