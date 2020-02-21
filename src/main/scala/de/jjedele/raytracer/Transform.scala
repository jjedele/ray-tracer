package de.jjedele.raytracer

/**
 * Transform projective coordinates.
 *
 * @param transforms
 */
class Transform(transforms: List[Matrix]) {

  import Matrix._

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

  /**
   * Scale point or vector by given factors.
   * @param sx
   * @param sy
   * @param sz
   * @return
   */
  def scale(sx: Double, sy: Double, sz: Double): Transform =
    new Transform(scalingMatrix(sx, sy, sz) +: transforms)

  /**
   * Rotate point around x-axis.
   * @param rad Angle in radians.
   * @return
   */
  def xRotate(rad: Double): Transform =
    new Transform(xRotationMatrix(rad) +: transforms)

  /**
   * Rotate point around y-axis.
   * @param rad Angle in radians.
   * @return
   */
  def yRotate(rad: Double): Transform =
    new Transform(yRotationMatrix(rad) +: transforms)

  /**
   * Rotate point around z-axis.
   * @param rad Angle in radians.
   * @return
   */
  def zRotate(rad: Double): Transform =
    new Transform(zRotationMatrix(rad) +: transforms)

  /**
   * Shear point.
   * @param xY Change of x in relation to y.
   * @param xZ Change of x in relation to z.
   * @param yX Change of y in relation to x.
   * @param yZ Change of y in relation to z.
   * @param zX Change of z in relation to x.
   * @param zY Change of z in relation to y.
   * @return
   */
  def shear(xY: Double = 0, xZ: Double = 0, yX: Double = 0, yZ: Double = 0, zX: Double = 0, zY: Double = 0): Transform =
    new Transform(shearingMatrix(xY, xZ, yX, yZ, zX, zY) +: transforms)

  private def translationMatrix(dx: Double, dy: Double, dz: Double): Matrix =
    matrix(
      Vector(1, 0, 0, dx),
      Vector(0, 1, 0, dy),
      Vector(0, 0, 1, dz),
      Vector(0, 0, 0, 1))

  private def scalingMatrix(sx: Double, sy: Double, sz: Double): Matrix =
    matrix(
      Vector(sx, 0, 0, 0),
      Vector(0, sy, 0, 0),
      Vector(0, 0, sz, 0),
      Vector(0, 0, 0, 1))

  private def xRotationMatrix(rad: Double): Matrix =
    matrix(
      Vector(1, 0, 0, 0),
      Vector(0, math.cos(rad), -math.sin(rad), 0),
      Vector(0, math.sin(rad), math.cos(rad), 0),
      Vector(0, 0, 0, 1))

  private def yRotationMatrix(rad: Double): Matrix =
    matrix(
      Vector(math.cos(rad), 0, math.sin(rad), 0),
      Vector(0, 1, 0, 0),
      Vector(-math.sin(rad), 0, math.cos(rad), 0),
      Vector(0, 0, 0, 1))

  private def zRotationMatrix(rad: Double): Matrix =
    matrix(
      Vector(math.cos(rad), -math.sin(rad), 0, 0),
      Vector(math.sin(rad), math.cos(rad), 0, 0),
      Vector(0, 0, 1, 0),
      Vector(0, 0, 0, 1))

  private def shearingMatrix(xY: Double, xZ: Double, yX: Double, yZ: Double, zX: Double, zY: Double): Matrix =
    matrix(
      Vector(1, xY, xZ, 0),
      Vector(yX, 1, yZ, 0),
      Vector(zX, zY, 1, 0),
      Vector(0, 0, 0, 1))

  override def equals(other: Any): Boolean = other match {
    case otherTransform: Transform => combined approximatelyEquals otherTransform.combined
    case _ => false
  }

}

object Transform {

  /**
   * @return Identity transformation.
   */
  def apply(): Transform =
    new Transform(List(Matrix.identityMatrix(4)))

}
