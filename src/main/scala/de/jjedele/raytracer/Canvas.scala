package de.jjedele.raytracer

import de.jjedele.raytracer.data.Matrix

/**
 * A canvas is a mutable 2D array of pixels.
 *
 * @param width
 * @param height
 * @param initialColor Initial color to fill the canvas background with.
 */
class Canvas(val height: Int, val width: Int, initialColor: Matrix = Matrix.color(0, 0, 0)) {

  val data = (0 until height).map { _ =>
    (0 until width).map{ _ =>
      initialColor
    }.toArray
  }.toArray

  /**
   * Access pixel at given coordinates.
   * @param row
   * @param column
   * @return
   */
  def apply(row: Int, column: Int): Matrix =
    data(row)(column)

  /**
   * Set pixel at given coordinates.
   * @param row
   * @param column
   * @param color
   */
  def update(row: Int, column: Int, color: Matrix): Unit =
    data(row)(column) = color

  /**
   * Paint on the canvas.
   *
   * This function uses inverted coordinates, i.e. (0,0) is bottom left.
   *
   * Also out-of-bounds errors are ignored and only indicated by the returned value.
   *
   * @param x
   * @param y
   * @param color
   * @return True if a pixel was updated, false if the coordinates were out of bounds.
   */
  def paint(x: Int, y: Int, color: Matrix): Boolean =
    if (x < 0 || x >= width || y < 0 || y >= height)
      false
    else {
      update(height - y - 1, x, color)
      true
    }

  /**
   * Serialize the canvas to Portable Pixmap (PPM) format.
   *
   * PPM is a simple, text-based image format.
   *
   * @return Serialized canvas.
   */
  def ppm: String = {
    def columnString(col: Array[Matrix]) = {
      // clip value to [0, 255]
      def clipTo(lower: Int, upper: Int)(x: Double): Double =
        Math.max(lower, Math.min(upper, x))

      // partition array of pixel strings such that no line exceeds maxChars
      def breakToMaxChars(strs: Seq[String], maxChars: Int): Seq[Seq[String]] = {
        val cumulativeLengths =
          strs
            .scanLeft(0)((acc, str) => acc + str.size + 1)
            .tail
        val partitions =
          (cumulativeLengths, strs)
            .zipped
            .groupBy {
             case (cumLength, _) => cumLength / maxChars
            }
        (0 until partitions.size).map { lineIdx =>
          partitions(lineIdx).map(_._2).toSeq
        }
      }

      val stringifiedPixels =
        col.flatMap { pixel =>
          (pixel * 255).data
            .map(clipTo(0, 255))
            .map(_.toInt)
            .map(_.toString)
        }

      breakToMaxChars(stringifiedPixels, maxChars = 70)
        .map(_.mkString(" "))
        .mkString("\n")
    }
    val canvasString = data.map(columnString).mkString("\n")
    s"""P3
       |$width $height
       |255
       |$canvasString
       |""".stripMargin
  }

}
