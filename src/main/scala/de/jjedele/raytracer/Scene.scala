package de.jjedele.raytracer

import de.jjedele.raytracer.lighting.PointLight
import de.jjedele.raytracer.objects.GeometricObject
import de.jjedele.raytracer.ray.{Intersections, Ray}

/**
 * A scene is a collection of objects and lights.
 * @param objects
 * @param lights
 */
case class Scene(objects: Set[GeometricObject], lights: Set[PointLight]) {

  def intersect(ray: Ray): Intersections =
    Intersections(objects.flatMap(_.intersect(ray).intersections).toSeq :_*)

}
