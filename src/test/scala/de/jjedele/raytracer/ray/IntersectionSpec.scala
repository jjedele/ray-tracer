package de.jjedele.raytracer.ray

import de.jjedele.raytracer.lighting.Material
import de.jjedele.raytracer.{Matrix, Transform}
import de.jjedele.raytracer.objects.{GeometricObject, Sphere}
import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntersectionSpec extends AnyFlatSpec with Matchers with MatrixMatchers with OptionValues {

  import Matrix._

  object DummyObject extends GeometricObject {
    override def intersect(ray: Ray): Intersections =
      Intersections()

    override def transform: Transform = Transform()

    override def withTransform(transform: Transform): GeometricObject = this

    override def normalAt(point: Matrix): Matrix = ???

    override def material: Material = ???
  }

  val dummyRay = Ray(
    point(0, 0, 0),
    direction(1, 1, 1))

  "The Intersections object" should "return the closest object intersection as hit if all have positive t values" in {
    val intersections = Intersections(
      Intersection(1, dummyRay, DummyObject),
      Intersection(2, dummyRay, DummyObject))

    val hit = intersections.hit

    hit.value shouldBe intersections(0)
  }

  it should "return the closest object intersection as hit if some have negative t values" in {
    val intersections = Intersections(
      Intersection(-1, dummyRay, DummyObject),
      Intersection(1, dummyRay, DummyObject))

    val hit = intersections.hit

    hit.value shouldBe intersections(1)
  }

  it should "return no hit if all have negative t values" in {
    val intersections = Intersections(
      Intersection(-2, dummyRay, DummyObject),
      Intersection(-1, dummyRay, DummyObject))

    val hit = intersections.hit

    hit shouldBe None
  }

  it should "always return the lowest non-negative intersection as hit" in {
    val intersections = Intersections(
      Intersection(5, dummyRay, DummyObject),
      Intersection(7, dummyRay, DummyObject),
      Intersection(-3, dummyRay, DummyObject),
      Intersection(2, dummyRay, DummyObject))

    val hit = intersections.hit

    hit.value shouldBe intersections(3)
  }

  it should "compute its properties" in {
    val ray = Ray(point(0, 0, -5), direction(0, 0, 1))
    val sphere = Sphere()

    val intersection = Intersection(4, ray, sphere)

    intersection.point should approximatelyEqual (point(0, 0, -1))
    intersection.eyeVector should approximatelyEqual (direction(0, 0, -1))
    intersection.normal should approximatelyEqual (direction(0, 0, -1))
  }

  it should "determine if a hit is outside of an object" in {
    val ray = Ray(point(0, 0, -5), direction(0, 0, 1))
    val sphere = Sphere()

    val intersection = Intersection(4, ray, sphere)

    intersection.inside shouldBe false
  }

  it should "determine if a hit is inside of an object" in {
    val ray = Ray(point(0, 0, 0), direction(0, 0, 1))
    val sphere = Sphere()

    val intersection = Intersection(1, ray, sphere)

    intersection.inside shouldBe true

    // would have been [0, 0, 1] but is inverted because we are inside the object
    intersection.normal should approximatelyEqual (direction(0, 0, -1))
  }

}
