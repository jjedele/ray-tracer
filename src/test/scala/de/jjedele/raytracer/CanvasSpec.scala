package de.jjedele.raytracer

import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CanvasSpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import Matrix._

  "A canvas" should "be initialized completely black" in {
    val height = 10
    val width = 20
    val black = color(0, 0, 0)
    val canvas = new Canvas(height, width, initialColor = black)

    (0 until height).map { row =>
      (0 until width).map { column =>
        canvas(row, column) should approximatelyEqual (black)
      }
    }
  }

  it should "be writable" in {
    val black = color(0, 0, 0)
    val red = color(1, 0, 0)

    val canvas = new Canvas(height = 10, width = 20, initialColor = black)

    canvas(2, 3) = red

    canvas(2, 3) should approximatelyEqual (red)
    canvas(2, 4) should approximatelyEqual (black)
  }

  it should "be paintable" in {
    val canvas = new Canvas(height = 5, width = 3)

    val red = color(1, 0, 0)
    canvas.paint(0, 0, red) shouldBe true
    canvas.paint(2, 4, red) shouldBe true
    canvas.paint(3, 5, red) shouldBe false

    canvas(4, 0) should approximatelyEqual (red)
    canvas(0, 2) should approximatelyEqual (red)
  }

  it should "serialize to Portable Pixmap (PPM) format" in {
    val canvas = new Canvas(height = 3, width = 5)

    val color1 = color(1.5, 0, 0)
    val color2 = color(0, 0.5, 0)
    val color3 = color(-0.5, 0, 1)

    canvas(0, 0) = color1
    canvas(1, 2) = color2
    canvas(2, 4) = color3

    canvas.ppm shouldBe
    s"""P3
       |5 3
       |255
       |255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       |0 0 0 0 0 0 0 127 0 0 0 0 0 0 0
       |0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
       |""".stripMargin


    val canvas2 = new Canvas(height= 2, width = 10)
    val color4 = color(1, 0.8, 0.6)
    (0 until 2).foreach { row =>
      (0 until 10).foreach { column =>
        canvas2(row, column) = color4
      }
    }

    canvas2.ppm shouldBe
      s"""P3
         |10 2
         |255
         |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
         |153 255 204 153 255 204 153 255 204 153 255 204 153
         |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
         |153 255 204 153 255 204 153 255 204 153 255 204 153
         |""".stripMargin
  }

}
