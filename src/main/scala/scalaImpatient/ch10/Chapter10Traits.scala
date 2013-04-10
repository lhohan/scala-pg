package scalaImpatient.ch10

import java.awt.geom.Ellipse2D

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

}
