package de.jjedele.raytracer

import de.jjedele.raytracer.lighting.{Illuminatable, PointLight}
import de.jjedele.raytracer.objects.{GeometricObject, Intersectable}
import de.jjedele.raytracer.ray.{Intersections, Ray}

/**
 * A scene is a collection of objects and lights.
 * @param objects
 * @param lights
 */
case class Scene(objects: Set[GeometricObject], lights: Set[PointLight]) extends Intersectable with Illuminatable {

  /**
   * Intersect scene with a ray.
   * @param ray
   * @return The intersection points.
   */
  def intersect(ray: Ray): Intersections =
    Intersections(objects.flatMap(_.intersect(ray).intersections).toSeq :_*)

  def colorFor(ray: Ray): Matrix =
    lights.map(light => colorFor(ray, light)).reduce(_ + _)

}
