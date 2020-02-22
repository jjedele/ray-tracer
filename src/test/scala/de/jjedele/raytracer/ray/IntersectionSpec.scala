package de.jjedele.raytracer.ray

import de.jjedele.raytracer.lighting.Material
import de.jjedele.raytracer.{Matrix, Transform}
import de.jjedele.raytracer.objects.GeometricObject
import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntersectionSpec extends AnyFlatSpec with Matchers with MatrixMatchers with OptionValues {

  object DummyObject extends GeometricObject {
    override def intersect(ray: Ray): Intersections =
      Intersections()

    override def transform: Transform = Transform()

    override def withTransform(transform: Transform): GeometricObject = this

    override def normalAt(point: Matrix): Matrix = ???

    override def material: Material = ???
  }

  "The Intersections object" should "return the closest object intersection as hit if all have positive t values" in {
    val intersections = Intersections(
      Intersection(1, DummyObject),
      Intersection(2, DummyObject))

    val hit = intersections.hit

    hit.value shouldBe intersections(0)
  }

  it should "return the closest object intersection as hit if some have negative t values" in {
    val intersections = Intersections(
      Intersection(-1, DummyObject),
      Intersection(1, DummyObject))

    val hit = intersections.hit

    hit.value shouldBe intersections(1)
  }

  it should "return no hit if all have negative t values" in {
    val intersections = Intersections(
      Intersection(-2, DummyObject),
      Intersection(-1, DummyObject))

    val hit = intersections.hit

    hit shouldBe None
  }

  it should "always return the lowest non-negative intersection as hit" in {
    val intersections = Intersections(
      Intersection(5, DummyObject),
      Intersection(7, DummyObject),
      Intersection(-3, DummyObject),
      Intersection(2, DummyObject))

    val hit = intersections.hit

    hit.value shouldBe intersections(3)
  }

}
