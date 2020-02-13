package de.jjedele.raytracer.data


/**
 * Our representation for matrices and vectors.
 *
 * Mixing these two together is not really nice, but we do not want
 * to develop a full-blown linear algebra library here.
 *
 * @param rows Number of rows.
 * @param columns Number of columns.
 * @param data Data.
 * @param isTransposed If row and column indices are switched.
 */
class Matrix(val rows: Int, val columns: Int, val data: Array[Double], val isTransposed: Boolean = false) {

  require(rows > 0)
  require(columns > 0)

  /**
   * Vector constructor.
   *
   * @param elements elements of vector.
   */
  def this(elements: Double*) = this(elements.size, 1, elements.toArray)

  /**
   * Access an element.
   *
   * @param row Row index.
   * @param column Column index.
   * @return Element.
   */
  def apply(row: Int, column: Int = 0): Double =
    if (isTransposed)
      data(row + column * rows)
    else
      data(row * columns + column)

  /**
   * Transpose matrix.
   *
   * @return Matrix such that row and column indexes are swapped.
   */
  def transposed: Matrix = new Matrix(columns, rows, data, !isTransposed)

  /**
   * Calculate matrix product.
   *
   * @param other Other matrix.
   * @return
   */
  def matmul(other: Matrix): Matrix = {
    assume(this.columns == other.rows)
    val resultRows = this.rows
    val resultColumns = other.columns

    def sourceIndexesForCell(r: Int, c: Int): Seq[((Int, Int), (Int, Int))] =
      Range(0, this.columns).map(ci => (r, ci))
        .zip(Range(0, other.rows).map(ri => (ri, c)))

    val resultIndexes = for (r <- Range(0, resultRows); c <- Range(0, resultColumns)) yield (r, c)
    val resultData = resultIndexes
      .map(Function.tupled(sourceIndexesForCell))
      .map { sourceIndexes =>
        sourceIndexes.map {
          case ((tr, tc), (or, oc)) => this(tr, tc) * other(or, oc)
        }.sum
      }

    new Matrix(resultRows, resultColumns, resultData.toArray)
  }

  /**
   * Calculate inner product of two matrices.
   *
   * @param other Other matrix.
   * @return Inner product.
   */
  def inner(other: Matrix): Double = {
    require(rows == other.rows && columns == other.columns, "dimensions do not match")

    (data, other.data).zipped.map(_ * _).sum
  }

  /**
   * @return Readable representation.
   */
  override def toString: String = Range(0, rows).map { row =>
    Range(0, columns).map { col => this(row, col) }.mkString("[", ",", "]")
  }.mkString("[", ",\n", "]")

  /**
   * Apply function element-wise.
   *
   * @param f Function to apply.
   * @return New matrix.
   */
  def map(f: Double => Double): Matrix =
    new Matrix(rows, columns, data.map(f), isTransposed)

  /**
   * Apply an operation element-wise to combine 2 matrices.
   *
   * @param other Second vector.
   * @param f Element-wise combination function.
   * @return New matrix containing the result.
   */
  def zipWith(other: Matrix, f: (Double, Double) => Double): Matrix = {
    assume(this.rows == other.rows)
    assume(this.columns == other.columns)
    new Matrix(rows, columns, (data, other.data).zipped.map(f).toArray, isTransposed)
  }

  /**
   * Compare two matrices for approximate equality.
   *
   * @param other Other matrix.
   * @return True if the matrices do not differ by more than a tiny error in total.
   */
  def approximatelyEquals(other: Matrix): Boolean =
    zipWith(other, (a, b) => Math.abs(a - b)).data.sum < 1e-4

  /**
   * Element-wise add 2 matrices.
   *
   * @param other Other matrix.
   * @return New matrix representing the sum.
   */
  def +(other: Matrix): Matrix =
    zipWith(other, _ + _)

  /**
   * Element-wise subtract 2 matrices.
   *
   * @param other Other matrix.
   * @return New matrix representing the difference.
   */
  def -(other: Matrix): Matrix =
    zipWith(other, _ - _)

  /**
   * @return Negated matrix.
   */
  def unary_-(): Matrix = map(x => -x)

  /**
   * Scalar multiplication.
   *
   * @param scalar
   * @return Scaled matrix.
   */
  def *(scalar: Double): Matrix =
    map(_ * scalar)

  /**
   * Scalar division.
   *
   * @param scalar
   * @return Scaled matrix.
   */
  def /(scalar: Double): Matrix =
    map(_ / scalar)

  /**
   * @return Magnitude of the matrix.
   */
  def magnitude: Double =
    Math.sqrt(data.map(Math.pow(_, 2)).sum)

  /**
   * @return Matrix normalized to unit magnitude.
   */
  def normalized: Matrix =
    this / magnitude

  /**
   * Calculate cross product of two vectors.
   *
   * The cross product is perpendicular to the two given vectors.
   * It is not commutative.
   *
   * This operation is only defined for size 4 column vectors.
   *
   * @param other Other vector.
   * @return Cross product.
   */
  def cross(other: Matrix): Matrix = {
    require(isProjectiveDirection)
    new Matrix(
      this(1) * other(2) - this(2) * other(1),
      this(2) * other(0) - this(0) * other(2),
      this(0) * other(1) - this(1) * other(0),
      this(3))
  }

  /**
   * @return True if matrix is a vector.
   */
  def isVector: Boolean = isColumnVector || isRowVector

  /**
   * @return True if vector is a column vector.
   */
  def isColumnVector: Boolean = columns == 1

  /**
   * @return True if vector is a row vector.
   */
  def isRowVector: Boolean = rows == 1

  /**
   * @return True if this matrix is a 4d column vector representing a direction in projective space.
   */
  def isProjectiveDirection: Boolean = isColumnVector && rows == 4 && this(3) == 0

  /**
   * @return True if this matrix is a 4d column vector representing a point in projective space.
   */
  def isProjectivePoint: Boolean = isColumnVector && rows == 4 && this(3) == 1

}

object Matrix {

  // TODO this is not super nice, but practical here
  type Vector = Matrix

  /**
   * Create a column vector out of given elements.
   * @param elements
   * @return Column vector containing elements.
   */
  def columnVector(elements: Double*): Vector = new Matrix(elements.size, 1, elements.toArray)

  /**
   * Create a vector representing a point in projective space.
   * @param x X-coordinate.
   * @param y Y-coordinate.
   * @param z Z-coordinate.
   * @return Vector.
   */
  def point(x: Double, y: Double, z: Double): Vector = columnVector(x, y, z, 1.0)

  /**
   * Create a vector representing a direction in projective space.
   * @param dx X-coordinate.
   * @param dy Y-coordinate.
   * @param dz Z-coordinate.
   * @return Vector.
   */
  def direction(dx: Double, dy: Double, dz: Double): Vector = columnVector(dx, dy, dz, 0.0)

  def matrix(data: Seq[Double]*): Matrix = {
    require(data.nonEmpty, "matrix is empty")
    require(data.map(_.length).forall(_ == data.head.length), "rows are not of equal length")
    new Matrix(data.size, data(0).size, data.flatMap(identity).toArray)
  }

}
