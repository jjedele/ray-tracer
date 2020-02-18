package de.jjedele.raytracer.misc

import java.io.PrintWriter

import de.jjedele.raytracer.{Canvas, Transform}

/**
 * Example from chapter 4 which generates a clock face.
 */
object ClockFace {

  import de.jjedele.raytracer.Matrix._

  def main(args: Array[String]): Unit = {
    val canvas = new Canvas(500, 500)
    val white = color(1, 1, 1)

    val p = point(0, 1, 0)
    (1 to 12)
      .map { h => Transform()
        .zRotate(2 * math.Pi / 12 * h)
        .scale(200, 200, 0)
        .translate(250, 250, 0)
        .apply(p) }
      .foreach { p =>
        canvas.paint(p(0).toInt, p(1).toInt, white)
      }

    val pw = new PrintWriter("clock.ppm")
    pw.write(canvas.ppm)
    pw.close()
  }

}
