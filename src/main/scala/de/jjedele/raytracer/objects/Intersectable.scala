package de.jjedele.raytracer.objects

import de.jjedele.raytracer.ray.{Intersections, Ray}

/**
 * Something that can be intersected by a ray.
 */
trait Intersectable {

  /**
   * Intersect the object with given ray.
   * @param ray
   * @return The intersection points.
   */
  def intersect(ray: Ray): Intersections

}
