package de.jjedele.raytracer

import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MatrixSpec extends AnyFlatSpec with Matchers with MatrixMatchers with OptionValues {

  import Matrix._
  import math._

  "A matrix" should "allow to access its elements" in {
    val m = matrix(
      Seq(1, 2),
      Seq(3, 4),
      Seq(5, 6)
    )

    m(0, 1) shouldBe 2
    m(2, 0) shouldBe 5
  }

  it should "transpose" in {
    val m = matrix(
      Vector(0, 9, 3, 0),
      Vector(9, 8, 0, 8),
      Vector(1, 8, 5, 3),
      Vector(0, 0, 5, 8)
    )
    val mt = matrix(
      Vector(0, 9, 1, 0),
      Vector(9, 8, 8, 0),
      Vector(3, 0, 5, 5),
      Vector(0, 8, 3, 8)
    )

    m.transposed should approximatelyEqual (mt)
    mt.transposed should approximatelyEqual (m)
    m.transposed.transposed should approximatelyEqual (m)
  }

  it should "multiply with another matrix" in {
    val a = matrix(
      Seq(1, 2, 3),
      Seq(4, 5, 6)
    )
    val b = columnVector(7, 8, 9)

    a matmul b should approximatelyEqual (columnVector(50, 122))
  }

  it should "multiply element-wise" in {
    val c1 = color(1, 0.2, 0.4)
    val c2 = color(0.9, 1, 0.1)

    c1 hadamard c2 should approximatelyEqual (color(0.9, 0.2, 0.04))
  }

  it should "be invariant to multiplications with the identity matrix" in {
    val m = matrix(
      Vector(0, 1, 2, 4),
      Vector(1, 2, 4, 8),
      Vector(2, 4, 8, 16),
      Vector(4, 8, 16, 32)
    )
    val eye = identityMatrix(4)

    m matmul eye should approximatelyEqual (m)
    eye matmul m should approximatelyEqual (m)

    val v = columnVector(1, 2, 3, 4)
    eye matmul v should approximatelyEqual (v)
  }

  it should "return its submatrices" in {
    val m1 = matrix(
      Vector(1, 5, 0),
      Vector(-3, 2, 7),
      Vector(0, 6, -3)
    )
    val sm1 = matrix(
      Vector(-3, 2),
      Vector(0, 6)
    )

    m1.submatrix(0, 2) should approximatelyEqual (sm1)

    val m2 = matrix(
      Vector(-6, 1, 1, 6),
      Vector(-8, 5, 8, 6),
      Vector(-1, 0, 8, 2),
      Vector(-7, 1, -1, 1)
    )
    val sm2 = matrix(
      Vector(-6, 1, 6),
      Vector(-8, 8, 6),
      Vector(-7, -1, 1)
    )

    m2.submatrix(2, 1) should approximatelyEqual (sm2)
  }

  it should "calculate its minors and cofactors" in {
    val m = matrix(
      Vector(3, 5, 0),
      Vector(2, -1, -7),
      Vector(6, -1, 5)
    )

    m.minor(0, 0) shouldBe -12.0+-1e-4
    m.cofactor(0, 0) shouldBe -12.0+-1e-4
    m.minor(1, 0) shouldBe 25.0+-1e-4
    m.cofactor(1, 0) shouldBe -25.0+-1e-4
  }

  it should "calculate its determinant" in {
    val m = matrix(
      Vector(1, 5),
      Vector(-3, 2)
    )

    m.determinant shouldBe 17.0+-1e-4

    val m2 = matrix(
      Vector(1, 2, 6),
      Vector(-5, 8, -4),
      Vector(2, 6, 4)
    )

    m2.determinant shouldBe -196.0+-1e-4

    val m3 = matrix(
      Vector(-2, -8, 3, 5),
      Vector(-3, 1, 7, 3),
      Vector(1, 2, -9, 6),
      Vector(-6, 7, 7, -9)
    )

    m3.determinant shouldBe -4071.0+-1e-4
  }

  it should "calculate its inverse" in {
    val m = matrix(
      Vector(-4, 2, -2, -3),
      Vector(9, 6, 2, 6),
      Vector(0, -5, 1, -5),
      Vector(0, 0, 0, 0)
    )

    m.inverse shouldBe None

    val m2 = matrix(
      Vector(-5, 2, 6, -8),
      Vector(1, -5, 1, 8),
      Vector(7, 7, -6, -7),
      Vector(1, -3, 7, 4)
    )

    val m2i = matrix(
      Vector(0.21805, 0.45113, 0.24060, -0.04511),
      Vector(-0.80827, -1.45677, -0.44361, 0.52068),
      Vector(-0.07895, -0.22368, -0.05263, 0.19737),
      Vector(-0.52256, -0.81391, -0.30075, 0.30639)
    )

    m2.inverse.value should approximatelyEqual (m2i)

    val m3 = matrix(
      Vector(8, -5, 9, 2),
      Vector(7, 5, 6, 1),
      Vector(-6, 0, 9, 6),
      Vector(-3, 0, -9, -4)
    )

    val m3i = matrix(
      Vector(-0.15385, -0.15385, -0.28205, -0.53846),
      Vector(-0.07692, 0.12308, 0.02564, 0.03077),
      Vector(0.35897, 0.35897, 0.43590, 0.92308),
      Vector(-0.69231, -0.69231, -0.76923, -1.92308),
    )

    m3.inverse.value should approximatelyEqual (m3i)

    val m4 = matrix(
      Vector(9, 3, 0, 9),
      Vector(-5, -2, -6, -3),
      Vector(-4, 9, 6, 4),
      Vector(-7, 6, 6, 2),
    )

    val m4i = matrix(
      Vector(-0.04074, -0.07778, 0.14444, -0.22222),
      Vector(-0.07778, 0.03333, 0.36667, -0.33333),
      Vector(-0.02901, -0.14630, -0.10926, 0.12963),
      Vector(0.17778, 0.06667, -0.26667, 0.33333),
    )

    m4.inverse.value should approximatelyEqual (m4i)

    m2 matmul m3 matmul m3.inverse.value should approximatelyEqual (m2)
    m2.inverse.value matmul m2 should approximatelyEqual (identityMatrix(4))
  }

  "A vector" should "identify as point if it is one" in {
    point(1, 1, 1).isProjectivePoint shouldBe true
  }

  it should "identify as direction if it is one" in {
    direction(1, 1, 1).isProjectiveDirection shouldBe true
  }

  it should "add for point and vector" in {
    val p = point(3, -2, 5)
    val v = direction(-2, 3, 1)

    // point + direction results in point
    (p + v) should approximatelyEqual (point(1, 1, 6))
  }

  it should "add correctly for 2 vectors" in {
    val v1 = direction(3, -2, 5)
    val v2 = direction(-2, 3, 1)

    // vector + vector results in vector
    (v1 + v2) should approximatelyEqual (direction(1, 1, 6))
  }

  it should "subtract for 2 points" in {
    val p1 = point(3, 2, 1)
    val p2 = point(5, 6, 7)

    // point - point results in a vector
    (p1 - p2) should approximatelyEqual (direction(-2, -4, -6))
  }

  it should "subtract for point and vector" in {
    val p = point(3, 2, 1)
    val v = direction(5, 6, 7)

    // point - vector results in point
    (p - v) should approximatelyEqual (point(-2, -4, -6))
  }

  it should "negate" in {
    val v = direction(1, -2, 3)

    -v should approximatelyEqual (direction(-1, 2, -3))
  }

  it should "multiply with a scalar" in {
    val v = direction(1, -2, 3)

    v * 3.5 should approximatelyEqual (direction(3.5, -7, 10.5))
    v * 0.5 should approximatelyEqual (direction(0.5, -1, 1.5))
  }

  it should "divide by a scalar" in {
    direction(1, -2, 3) / 2 should approximatelyEqual (direction(0.5, -1, 1.5))
  }

  it should "return the magnitude" in {
    direction(1, 0, 0).magnitude shouldBe 1
    direction(0, 1, 0).magnitude shouldBe 1
    direction(0, 0, 1).magnitude shouldBe 1
    direction(1, 2, 3).magnitude shouldBe Math.sqrt(14) +- 1e-4
    direction(-1, -2, -3).magnitude shouldBe Math.sqrt(14) +- 1e-4
  }

  it should "normalize to unit length" in {
    direction(4, 0, 0).normalized should approximatelyEqual (direction(1, 0, 0))
    direction(1, 2, 3).normalized should approximatelyEqual(direction(0.26726, 0.53452, 0.80178))

    direction(1, 2, 3).normalized.magnitude shouldBe 1.0 +- 1e-4
  }

  it should "compute dot products" in {
    direction(1, 2, 3) inner direction(2, 3, 4) shouldBe 20.0 +- 1e-4
  }

  it should "compute cross products" in {
    direction(1, 2, 3) cross direction(2, 3, 4) should approximatelyEqual (direction(-1, 2, -1))
    direction(2, 3, 4) cross direction(1, 2, 3) should approximatelyEqual (direction(1, -2, 1))
  }

  it should "reflect at surfaces" in {
    val vector = direction(0, -1, 0)
    val surfaceNormal = direction(sqrt(2) / 2, sqrt(2) / 2, 0)

    vector.reflectAt(surfaceNormal) should approximatelyEqual (direction(1, 0, 0))
  }

}
