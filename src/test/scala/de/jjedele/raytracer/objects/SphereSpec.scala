package de.jjedele.raytracer.objects

import de.jjedele.raytracer
import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import de.jjedele.raytracer.{Matrix, Transform}
import de.jjedele.raytracer.ray.{Intersection, Intersections, Ray}

class SphereSpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import Matrix._

  "A sphere" should "intersect with a ray in 2 points" in {
    val ray = raytracer.ray.Ray(point(0, 0, -5), direction(0, 0, 1))
    val sphere = Sphere()

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(4.0, sphere),
      Intersection(6.0, sphere))
  }

  it should "be intersected by a ray at a tangent" in {
    val ray = raytracer.ray.Ray(point(0, 1, -5), direction(0, 0, 1))
    val sphere = Sphere()

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(5.0, sphere),
      Intersection(5.0, sphere))
  }

  it should "be missed by non-intersecting rays" in {
    val ray = raytracer.ray.Ray(point(0, 2, -5), direction(0, 0, 1))
    val sphere = Sphere()

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections()
  }

  it should "be intersected by rays originating inside it" in {
    val ray = raytracer.ray.Ray(point(0, 0, 0), direction(0, 0, 1))
    val sphere = Sphere()

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(-1.0, sphere),
      Intersection(+1.0, sphere))
  }

  it should "be intersected by rays originating behind it" in {
    val ray = raytracer.ray.Ray(point(0, 0, 5), direction(0, 0, 1))
    val sphere = Sphere()

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(-6.0, sphere),
      Intersection(-4.0, sphere))
  }

  it should "have a identity transformation as default" in {
    val sphere = Sphere()

    sphere.transform shouldBe Transform()
  }

  it should "support changing its transformation" in {
    val sphere = Sphere()
    val transform = Transform().translate(2, 3, 4)

    val sphere2 = sphere.copy(transform=transform)

    sphere2.transform shouldBe transform
  }

  it should "intersect with a ray when scaled" in {
    val ray = raytracer.ray.Ray(point(0, 0, -5), direction(0, 0, 1))
    val sphere = Sphere()
      .withTransform(Transform().scale(2, 2, 2))

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections(
      Intersection(3, sphere),
      Intersection(7, sphere))
  }

  it should "intersect with a ray when translated" in {
    val ray = raytracer.ray.Ray(point(0, 0, -5), direction(0, 0, 1))
    val sphere = Sphere()
      .withTransform(Transform().translate(5, 0, 0))

    val intersections = sphere.intersect(ray)

    intersections shouldBe Intersections()
  }

}
