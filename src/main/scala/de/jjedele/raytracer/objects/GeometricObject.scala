package de.jjedele.raytracer.objects

import de.jjedele.raytracer.lighting.Material
import de.jjedele.raytracer.{Matrix, Transform}

/**
 * Base trait for geometric objects.
 */
trait GeometricObject extends Intersectable {

  /**
   * @return Transformation associated with this object.
   */
  def transform: Transform

  /**
   * Change transformation associated with this object.
   * @param transform
   * @return
   */
  def withTransform(transform: Transform): GeometricObject

  /**
   * Calculate the surface normal at given point.
   * @param point Point is assumed to be on the object. No further checks are done.
   * @return
   */
  def normalAt(point: Matrix): Matrix

  /**
   * @return The material associated with the object.
   */
  def material: Material

}
