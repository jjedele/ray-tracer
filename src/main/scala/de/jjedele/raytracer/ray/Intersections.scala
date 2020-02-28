package de.jjedele.raytracer.ray

import de.jjedele.raytracer.Matrix
import de.jjedele.raytracer.objects.GeometricObject

/**
 * Intersection of a ray with a geometric object.
 * @param ray The intersecting ray.
 * @param t Distance units from the ray origin.
 * @param obj Intersected object.
 */
case class Intersection(t: Double, ray: Ray, obj: GeometricObject) {

  /** The point of the intersection. */
  lazy val point: Matrix = ray(t)

  /** Direction of the eye. */
  lazy val eyeVector: Matrix = -ray.direction

  private lazy val rawNormal = obj.normalAt(point)

  /** True if hit is inside the object. */
  lazy val inside: Boolean = rawNormal.inner(eyeVector) < 0

  /** Surface normal at the intersection point. */
  lazy val normal: Matrix =
    if (inside)
      -rawNormal
    else
      rawNormal

}

/**
 * Collection of intersections.
 * @param intersections
 */
case class Intersections(intersections: Intersection*) {

  /**
   * Intersections sorted ascending by distance from the
   * ray origin.
   */
  lazy val sorted: Seq[Intersection] =
    intersections
      .sortBy(_.t)

  /**
   * Get specific intersection.
   * @param i
   * @return
   */
  def apply(i: Int): Intersection = intersections(i)

  /**
   * Return the intersection closest to the ray's origin.
   *
   * Intersections with negative distance units are not considered.
   * @return
   */
  def hit: Option[Intersection] =
    sorted
      .filter(_.t >= 0)
      .headOption

}
