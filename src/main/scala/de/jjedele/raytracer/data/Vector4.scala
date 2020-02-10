package de.jjedele.raytracer.data

/**
 * A 4-dimensional vector used for projective geometry.
 */
case class Vector4 private (data: Array[Double]) extends AnyVal {

  /**
   * @return True if vector represents a point in space, i.e. w coordinate is 1.
   */
  def isPoint: Boolean = data(3) == 1

  /**
   * @return True if vector represents a direction and distance, i.e. w coordinate is 0.
   */
  def isVector: Boolean = data(3) == 0

  /**
   * Apply an operation element-wise to combine 2 vectors.
   *
   * @param other The second vector.
   * @param f The element-wise combination function.
   * @return A new vector containing the result.
   */
  def applyElementWise(other: Vector4, f: (Double, Double) => Double): Vector4 =
    Vector4((data, other.data).zipped.map(f).toArray)

  /**
   * Element-wise add 2 vectors.
   *
   * @param other The other vector.
   * @return A new vector representing the sum.
   */
  def +(other: Vector4): Vector4 =
    applyElementWise(other, _ + _)

  /**
   * Element-wise subtract 2 vectors.
   *
   * @param other The other vector.
   * @return A new vector representing the difference.
   */
  def -(other: Vector4): Vector4 =
    applyElementWise(other, _ - _)

  /**
   * Compare two vectors for approximate equality.
   *
   * @param other The other vector.
   * @return True if the vector coordinates do not differ by more than a tiny error.
   */
  def approximatelyEquals(other: Vector4): Boolean =
    applyElementWise(other, absoluteDifference).data.sum < Vector4.Epsilon

  private def absoluteDifference(a: Double, b: Double): Double = Math.abs(b - a)

}

/**
 * Helper functions for projective geometry vectors.
 */
object Vector4 {

  private val Epsilon = 1e-8

  private def apply(x: Double, y: Double, z: Double, w: Double): Vector4 = Vector4(Array(x, y, z, w))

  /**
   * Create a new vector representing a point.
   *
   * @param x X-coordinate.
   * @param y Y-coordinate.
   * @param z Z-coordinate.
   * @return New vector.
   */
  def point(x: Double, y: Double, z: Double): Vector4 = apply(x, y, z, 1)

  /**
   * Create a new vector representing direction and distance.
   *
   * @param x X-coordinate.
   * @param y Y-coordinate.
   * @param z Z-coordinate.
   * @return New vector.
   */
  def vector(x: Double, y: Double, z: Double): Vector4 = apply(x, y, z, 0)

}
