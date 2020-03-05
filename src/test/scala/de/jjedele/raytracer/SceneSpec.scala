package de.jjedele.raytracer

import de.jjedele.raytracer.lighting.{Material, PointLight}
import de.jjedele.raytracer.objects.Sphere
import de.jjedele.raytracer.ray.Ray
import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SceneSpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import Matrix._

  def defaultScene: Scene = {
    val light = PointLight(
      point(-10, 10, -10),
      color(1, 1, 1))

    val sphere1 = Sphere(material = Material(color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2))

    val sphere2 = Sphere()
      .withTransform(Transform().scale(0.5, 0.5, 0.5))

    Scene(Set(sphere1, sphere2), Set(light))
  }

  "A scene" should "intersect with a ray" in {
    val scene = defaultScene
    val ray = Ray(point(0, 0, -5), direction(0, 0, 1))

    val intersections = scene.intersect(ray)

    intersections.sorted.map(_.t) shouldBe Seq(4, 4.5, 5.5, 6)
  }

  it should "color an intersection" in {
    val scene = defaultScene
    val ray = Ray(point(0, 0, -5), direction(0, 0, 1))

    val expected = color(0.38066, 0.47583, 0.2855)
    scene.colorFor(ray) should approximatelyEqual (expected)
  }

  it should "color an intersection from the inside" in {
    val newLight = PointLight(
      point(0, 0.25, 0),
      color(1, 1, 1))
    val scene = defaultScene.copy(lights=Set(newLight))

    val ray = Ray(point(0, 0, 0), direction(0, 0, 1))

    val expected = color(0.90498, 0.90498, 0.90498)
    scene.colorFor(ray) should approximatelyEqual (expected)
  }

  it should "remain black when the ray misses all objects" in {
    val scene = defaultScene
    val ray = Ray(point(0, 0, -5), direction(0, 1, 0))

    scene.colorFor(ray) should approximatelyEqual (color(0, 0, 0))
  }

  it should "only use ambient lighting for intersections behind the ray" in {
    val light = PointLight(
      point(-10, 10, -10),
      color(1, 1, 1))

    val sphere1 = Sphere(material = Material(color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2, ambient = 1))

    val sphere2 = Sphere(material = Material().copy(ambient = 1))
      .withTransform(Transform().scale(0.5, 0.5, 0.5))

    val scene = Scene(Set(sphere1, sphere2), Set(light))

    val ray = Ray(point(0, 0, 0.75), direction(0, 0, -1))

    scene.colorFor(ray) should approximatelyEqual (sphere2.material.color)
  }

}
