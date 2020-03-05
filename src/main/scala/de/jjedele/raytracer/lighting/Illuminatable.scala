package de.jjedele.raytracer.lighting

import de.jjedele.raytracer.Matrix
import de.jjedele.raytracer.objects.Intersectable
import de.jjedele.raytracer.ray.Ray

/**
 * Objects that can be illuminated.
 */
trait Illuminatable {

  this: Intersectable =>

  /**
   * Return the color resulting for intersecting the object with a ray.
   * @param ray
   * @param light
   * @return
   */
  def colorFor(ray: Ray, light: PointLight): Matrix = {
    val intersections = intersect(ray)
    intersections.hit match {
      case Some(hit) => PhongLighting.apply(
        hit.obj.material,
        light,
        hit.point,
        hit.eyeVector,
        hit.normal)
      case None => Matrix.color(0, 0, 0)
    }
  }

}
