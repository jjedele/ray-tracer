package de.jjedele.raytracer

import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TransformSpec extends AnyFlatSpec with Matchers with MatrixMatchers with OptionValues {

  import Matrix._

  "The transformer" should "translate points" in {
    val p = point(-3, 4, 5)

    val t = Transform().translate(5, -3, 2)

    val pt = t(p)

    pt should approximatelyEqual (point(2, 1, 7))
  }

  it should "not translate vectors" in {
    val v = direction(-3, 4, 5)

    val t = Transform().translate(5, -3, 2)

    val vt = t(v)

    vt should approximatelyEqual (v)
  }

  it should "be undoable for translations" in {
    val p = point(-3, 4, 5)

    val t = Transform()
      .translate(5, -3, 2)
    val ti = t.inverse()

    val pt = t(p)

    ti.value.apply(pt) should approximatelyEqual (p)
  }

  it should "scale points and vectors" in {
    val p = point(-4, 6, 8)
    val v = direction(-4, 6, 8)

    val t = Transform()
      .scale(2, 3, 4)

    val pt = t(p)
    val vt = t(v)

    pt should approximatelyEqual (point(-8, 18, 32))
    vt should approximatelyEqual (direction(-8, 18, 32))

    val ti = t.inverse.value

    ti(pt) should approximatelyEqual (p)
    ti(vt) should approximatelyEqual (v)

    // reflection

    val rt = Transform()
      .scale(-1, 1, 1)

    rt(p) should approximatelyEqual (point(4, 6 ,8))
  }

  it should "rotate points around the x-axis" in {
    val p = point(0, 1, 0)

    val halfQuarterRotation = Transform()
      .xRotate(math.Pi / 4)
    val fullQuarterRotation = halfQuarterRotation
      .xRotate(math.Pi / 4)

    halfQuarterRotation(p) should approximatelyEqual (point(0, math.sqrt(2) / 2, math.sqrt(2) / 2))
    fullQuarterRotation(p) should approximatelyEqual (point(0, 0, 1))

    val halfQuarterRotationInv = halfQuarterRotation.inverse().value

    halfQuarterRotationInv(point(0, 0, 1)) should approximatelyEqual (point(0, math.sqrt(2) / 2, math.sqrt(2) / 2))
  }

  it should "rotate points around the y-axis" in {
    val p = point(0, 0, 1)

    val halfQuarterRotation = Transform()
      .yRotate(math.Pi / 4)
    val fullQuarterRotation = halfQuarterRotation
      .yRotate(math.Pi / 4)

    halfQuarterRotation(p) should approximatelyEqual (point(math.sqrt(2) / 2, 0, math.sqrt(2) / 2))
    fullQuarterRotation(p) should approximatelyEqual (point(1, 0, 0))
  }

  it should "rotate points around the z-axis" in {
    val p = point(0, 1, 0)

    val halfQuarterRotation = Transform()
      .zRotate(math.Pi / 4)
    val fullQuarterRotation = halfQuarterRotation
      .zRotate(math.Pi / 4)

    halfQuarterRotation(p) should approximatelyEqual (point(-math.sqrt(2) / 2, math.sqrt(2) / 2, 0))
    fullQuarterRotation(p) should approximatelyEqual (point(-1, 0, 0))
  }

}
