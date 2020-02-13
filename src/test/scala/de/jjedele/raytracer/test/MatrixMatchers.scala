package de.jjedele.raytracer.test

import de.jjedele.raytracer.data.Matrix
import org.scalatest.matchers.{MatchResult, Matcher}

trait MatrixMatchers {

  class ApproximatelyEqualsMatcher(expected: Matrix) extends Matcher[Matrix] {

    def apply(actual: Matrix) =
      MatchResult(
        actual approximatelyEquals expected,
        s"""$actual did not approximately equal $expected""",
        s"""$actual did approximately equal $expected""")

  }

  def approximatelyEqual(expected: Matrix) = new ApproximatelyEqualsMatcher(expected)

}
