package de.jjedele.raytracer.objects

import de.jjedele.raytracer.Matrix
import de.jjedele.raytracer.ray.{Intersection, Intersections, Ray}

/**
 * A sphere.
 * @param center
 * @param radius
 */
case class Sphere(center: Matrix, radius: Double) extends GeometricObject {

  /**
   * Intersect the object with given ray.
   *
   * @param ray
   * @return The intersection points.
   */
  override def intersect(ray: Ray): Intersections = {
    val sphereToRay = ray.origin - this.center

    val a = ray.direction inner ray.direction
    val b = ray.direction inner sphereToRay * 2
    val c = (sphereToRay inner sphereToRay) - 1

    val discriminant = math.pow(b, 2) - 4 * a * c

    if (discriminant < 0)
      Intersections()
    else
      Intersections(
        Intersection((-b - math.sqrt(discriminant)) / (2 * a), this),
        Intersection((-b + math.sqrt(discriminant)) / (2 * a), this))
  }

}
