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

    val t = Transform().translate(5, -3, 2)
    val ti = t.inverse()

    val pt = t(p)

    ti.value.apply(pt) should approximatelyEqual (p)
  }

}
