package de.jjedele.raytracer.data

import de.jjedele.raytracer.test.VectorMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Vector4Spec extends AnyFlatSpec with Matchers with VectorMatchers {

  "A Vector4" should "identify as a point if created with corresponding constructor" in {
    Vector4.point(1, 2, 3).isPoint shouldBe true
    Vector4.point(1, 2, 3).isVector shouldBe false
  }

  it should "identify as a vector if created with corresponding constructor" in {
    Vector4.vector(1, 2, 3).isVector shouldBe true
    Vector4.vector(1, 2, 3).isPoint shouldBe false
  }

  it should "add for point and vector" in {
    val p = Vector4.point(3, -2, 5)
    val v = Vector4.vector(-2, 3, 1)

    // point + vector results in point
    (p + v) should approximatelyEqual (Vector4.point(1, 1, 6))
  }

  it should "add correctly for 2 vectors" in {
    val v1 = Vector4.vector(3, -2, 5)
    val v2 = Vector4.vector(-2, 3, 1)

    // vector + vector results in vector
    (v1 + v2) should approximatelyEqual (Vector4.vector(1, 1, 6))
  }

  it should "subtract for 2 points" in {
    val p1 = Vector4.point(3, 2, 1)
    val p2 = Vector4.point(5, 6, 7)

    // point - point results in a vector
    (p1 - p2) should approximatelyEqual (Vector4.vector(-2, -4, -6))
  }

  it should "subtract for point and vector" in {
    val p = Vector4.point(3, 2, 1)
    val v = Vector4.vector(5, 6, 7)

    // point - vector results in point
    (p - v) should approximatelyEqual (Vector4.point(-2, -4, -6))
  }

  it should "negate" in {
    val p = Vector4.vector(1, -2, 3)

    -p should approximatelyEqual (Vector4.vector(-1, 2, -3))
  }

  it should "multiply with a scalar" in {
    val p = Vector4.vector(1, -2, 3)

    p * 3.5 should approximatelyEqual (Vector4.vector(3.5, -7, 10.5))
    p * 0.5 should approximatelyEqual (Vector4.vector(0.5, -1, 1.5))
  }

  it should "divide by a scalar" in {
    Vector4.vector(1, -2, 3) / 2 should approximatelyEqual (Vector4.vector(0.5, -1, 1.5))
  }

}
