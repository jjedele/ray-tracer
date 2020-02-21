package de.jjedele.raytracer.objects

import de.jjedele.raytracer.ray.{Intersections, Ray}

/**
 * Base trait for geometric objects.
 */
trait GeometricObject {

  /**
   * Intersect the object with given ray.
   * @param ray
   * @return The intersection points.
   */
  def intersect(ray: Ray): Intersections

}
