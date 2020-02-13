package de.jjedele.raytracer.data

import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MatrixSpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import Matrix._

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
      Seq(1, 2),
      Seq(3, 4),
      Seq(5, 6)
    )
    val mt = m.transposed

    mt(0, 2) shouldBe 5
    mt(1, 1) shouldBe 4

    mt.transposed should approximatelyEqual (m)
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

}
