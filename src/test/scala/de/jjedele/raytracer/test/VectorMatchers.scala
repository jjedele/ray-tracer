package de.jjedele.raytracer.test

import de.jjedele.raytracer.data.Vector4
import org.scalatest.matchers.{MatchResult, Matcher}

trait VectorMatchers {

  class ApproximatelyEqualsMatcher(expected: Vector4) extends Matcher[Vector4] {

    def apply(actual: Vector4) =
      MatchResult(
        actual approximatelyEquals expected,
        s"""$actual did not approximately equal $expected""",
        s"""$actual did approximately equal $expected""")

  }

  def approximatelyEqual(expected: Vector4) = new ApproximatelyEqualsMatcher(expected)

}
