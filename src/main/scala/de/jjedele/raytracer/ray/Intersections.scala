package de.jjedele.raytracer.ray

import de.jjedele.raytracer.objects.GeometricObject

/**
 * Intersection of a ray with a geometric object.
 * @param t Distance units from the ray origin.
 * @param obj Intersected object.
 */
case class Intersection(t: Double, obj: GeometricObject)

/**
 * Collection of intersections.
 * @param intersections
 */
case class Intersections(intersections: Intersection*) {

  /**
   * Get specific intersection.
   * @param i
   * @return
   */
  def apply(i: Int): Intersection = intersections(i)

}
