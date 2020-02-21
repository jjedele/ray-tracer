package de.jjedele.raytracer.objects

import de.jjedele.raytracer
import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import de.jjedele.raytracer.Matrix
import de.jjedele.raytracer.ray.{Intersection, Intersections, Ray}

class SphereSpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import Matrix._

  "A sphere" should "intersect with a ray in 2 points" in {
    val ray = raytracer.ray.Ray(point(0, 0, -5), direction(0, 0, 1))
    val sphere = Sphere(point(0, 0, 0), radius = 1)

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(4.0, sphere),
      Intersection(6.0, sphere))
  }

  it should "be intersected by a ray at a tangent" in {
    val ray = raytracer.ray.Ray(point(0, 1, -5), direction(0, 0, 1))
    val sphere = Sphere(point(0, 0, 0), radius = 1)

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(5.0, sphere),
      Intersection(5.0, sphere))
  }

  it should "be missed by non-intersecting rays" in {
    val ray = raytracer.ray.Ray(point(0, 2, -5), direction(0, 0, 1))
    val sphere = Sphere(point(0, 0, 0), radius = 1)

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections()
  }

  it should "be intersected by rays originating inside it" in {
    val ray = raytracer.ray.Ray(point(0, 0, 0), direction(0, 0, 1))
    val sphere = Sphere(point(0, 0, 0), radius = 1)

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(-1.0, sphere),
      Intersection(+1.0, sphere))
  }

  it should "be intersected by rays originating behind it" in {
    val ray = raytracer.ray.Ray(point(0, 0, 5), direction(0, 0, 1))
    val sphere = Sphere(point(0, 0, 0), radius = 1)

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(-6.0, sphere),
      Intersection(-4.0, sphere))
  }

}
