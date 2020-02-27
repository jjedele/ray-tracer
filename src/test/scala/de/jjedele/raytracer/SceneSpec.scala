package de.jjedele.raytracer

import de.jjedele.raytracer.lighting.{Material, PointLight}
import de.jjedele.raytracer.objects.Sphere
import de.jjedele.raytracer.ray.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SceneSpec extends AnyFlatSpec with Matchers {

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

}
