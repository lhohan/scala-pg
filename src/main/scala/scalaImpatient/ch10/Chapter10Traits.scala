package scalaImpatient.ch10

import java.awt.geom.Ellipse2D
import java.awt.Point

object Chapter10Traits {

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

}
