package de.jjedele.raytracer.ray

import de.jjedele.raytracer.{Matrix, Transform}

/**
 * A ray that has a direction and goes through an origin.
 *
 * @param origin
 * @param direction
 */
case class Ray (origin: Matrix, direction: Matrix) {

  /**
   * Get point on ray position units away from the origin.
   * @param position
   * @return
   */
  def apply(position: Double): Matrix =
    origin + direction * position

  /**
   * Transform ray by given transformation.
   * @param transform
   * @return
   */
  def transformBy(transform: Transform): Ray =
    Ray(transform(origin), transform(direction))

}
