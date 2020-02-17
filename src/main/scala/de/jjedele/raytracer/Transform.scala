package de.jjedele.raytracer

/**
 * Transform projective coordinates.
 *
 * @param transforms
 */
class Transform(transforms: List[Matrix]) {

  lazy val combined = transforms.reduceLeft(_ matmul _)

  /**
   * Apply transformation to vector.
   * @param target Vector must be a 4d column-vector representing a coordinate in projective space.
   * @return Transformed vector.
   */
  def apply(target: Matrix): Matrix =
    combined matmul target

  /**
   * @return Transformation which reverses the current one.
   */
  def inverse(): Option[Transform] =
    combined.inverse.map { m => new Transform(List(m)) }

  /**
   * Translate point by given offsets.
   * @param dx
   * @param dy
   * @param dz
   * @return
   */
  def translate(dx: Double, dy: Double, dz: Double): Transform =
    new Transform(translationMatrix(dx, dy, dz) +: transforms)

  private def translationMatrix(dx: Double, dy: Double, dz: Double): Matrix =
    Matrix.matrix(
      Vector(1, 0, 0, dx),
      Vector(0, 1, 0, dy),
      Vector(0, 0, 1, dz),
      Vector(0, 0, 0, 1))

}

object Transform {

  /**
   * @return Identity transformation.
   */
  def apply(): Transform =
    new Transform(List(Matrix.identityMatrix(4)))

}
