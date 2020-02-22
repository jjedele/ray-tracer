package de.jjedele.raytracer.misc

import java.io.PrintWriter

import de.jjedele.raytracer.lighting.{Material, PhongLighting, PointLight}
import de.jjedele.raytracer.objects.Sphere
import de.jjedele.raytracer.ray.Ray
import de.jjedele.raytracer.{Canvas, Matrix, Transform}

/**
 * This is the sphere example
 * from Chapter 6's bringing it together.
 */
object SphereWithLighting {

  import Matrix._

  def main(args: Array[String]): Unit = {
    val canvasPixels = 500
    val canvas = new Canvas(canvasPixels, canvasPixels)

    val light = PointLight(
      point(-10, 10, -10),
      color(1, 1, 1))

    val shape = Sphere()

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
            case Some(hit) => {
              val hitPoint = ray(hit.t)
              val normal = hit.obj.normalAt(hitPoint)
              val color = PhongLighting(hit.obj.material, light, hitPoint, -ray.direction, normal)
              canvas.paint(x, y, color)
            }
            case _ =>
          }
        }
      }

    val pw = new PrintWriter("sphere.ppm")
    pw.print(canvas.ppm)
    pw.close()
  }

}
