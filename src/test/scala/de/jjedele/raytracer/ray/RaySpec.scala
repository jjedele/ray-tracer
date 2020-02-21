package de.jjedele.raytracer.ray

import de.jjedele.raytracer.{Matrix, Transform}
import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RaySpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import Matrix._

  "A ray" should "return points defined distances away from the origin" in {
    val ray = Ray(point(2, 3, 4), direction(1, 0, 0))

    ray(0) should approximatelyEqual (ray.origin)
    ray(1) should approximatelyEqual (point(3, 3, 4))
    ray(-1) should approximatelyEqual (point(1, 3, 4))
    ray(2.5) should approximatelyEqual (point(4.5, 3, 4))
  }

  it should "be translatable" in {
    val ray = Ray(point(1, 2, 3), direction(0, 1, 0))

    val transformation = Transform().translate(3, 4, 5)

    val translatedRay = ray.transformBy(transformation)

    translatedRay.origin should approximatelyEqual (point(4, 6, 8))
    translatedRay.direction should approximatelyEqual (direction(0, 1, 0))
  }

  it should "be scalable" in {
    val ray = Ray(point(1, 2, 3), direction(0, 1, 0))

    val transformation = Transform().scale(2, 3, 4)

    val translatedRay = ray.transformBy(transformation)

    translatedRay.origin should approximatelyEqual (point(2, 6, 12))
    translatedRay.direction should approximatelyEqual (direction(0, 3, 0))
  }

}
