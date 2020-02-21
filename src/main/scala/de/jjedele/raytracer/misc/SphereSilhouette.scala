package de.jjedele.raytracer.misc

import java.io.PrintWriter

import de.jjedele.raytracer.objects.Sphere
import de.jjedele.raytracer.ray.Ray
import de.jjedele.raytracer.{Canvas, Matrix, Transform}

/**
 * This is the sphere silhouette example
 * from Chapter 5's bringing it together.
 */
object SphereSilhouette {

  import Matrix._

  def main(args: Array[String]): Unit = {
    val canvasPixels = 200
    val canvas = new Canvas(canvasPixels, canvasPixels)
    val paintColor = color(1, 0, 0)

    val shape = Sphere()
      .withTransform(Transform())

    val rayOrigin = point(0, 0, -5)
    val wallZ = 10
    val wallSize = 7.0

    val pixelSize = wallSize / canvasPixels
    val wallHalf = wallSize / 2

    val pixelCoordinates = for (x <- (0 until canvasPixels).view; y <- (0 until canvasPixels).view) yield (x, y)
    pixelCoordinates
      .foreach {
        case (x, y) => {
          val worldX = -wallHalf + pixelSize * x
          val worldY = -wallHalf + pixelSize * y

          val rayTarget = point(worldX, worldY, wallZ)
          val ray = Ray(rayOrigin, (rayTarget - rayOrigin).normalized)

          val intersections = shape.intersect(ray)
          intersections.hit match {
            case Some(hit) => canvas.paint(x, y, paintColor)
            case _ =>
          }
        }
      }

    val pw = new PrintWriter("sphere.ppm")
    pw.print(canvas.ppm)
    pw.close()
  }

}
