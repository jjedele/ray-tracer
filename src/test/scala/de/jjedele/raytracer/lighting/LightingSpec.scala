package de.jjedele.raytracer.lighting

import de.jjedele.raytracer.test.MatrixMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LightingSpec extends AnyFlatSpec with Matchers with MatrixMatchers {

  import de.jjedele.raytracer.Matrix._
  import math._

  "Lighting" should "work with eye between the light and the surface" in {
    val material = Material()
    val reflectionPoint = point(0, 0, 0)

    val eyeVector = direction(0, 0, -1)
    val surfaceNormal = direction(0, 0, -1)

    val light = PointLight(point(0, 0, -10), color(1, 1, 1))

    val resultingColor = PhongLighting(material, light, reflectionPoint, eyeVector, surfaceNormal)

    // all lighting components are full intensity
    resultingColor should approximatelyEqual (color(1.9, 1.9, 1.9))
  }

  it should "work with eye between light and surface, eye offset at 45°" in {
    val material = Material()
    val reflectionPoint = point(0, 0, 0)

    val eyeVector = direction(0, sqrt(2) / 2, -sqrt(2) / 2)
    val surfaceNormal = direction(0, 0, -1)

    val light = PointLight(point(0, 0, -10), color(1, 1, 1))

    val resultingColor = PhongLighting(material, light, reflectionPoint, eyeVector, surfaceNormal)

    // specular reflection should drop to almost 0
    resultingColor should approximatelyEqual (color(1, 1, 1))
  }

  it should "work with eye between light and surface, light offset at 45°" in {
    val material = Material()
    val reflectionPoint = point(0, 0, 0)

    val eyeVector = direction(0, 0, -1)
    val surfaceNormal = direction(0, 0, -1)

    val light = PointLight(point(0, 10, -10), color(1, 1, 1))

    val resultingColor = PhongLighting(material, light, reflectionPoint, eyeVector, surfaceNormal)

    // specular reflection should drop to almost 0
    resultingColor should approximatelyEqual (color(0.7364, 0.7364, 0.7364))
  }

  it should "work with eye in the path of the reflection vector" in {
    val material = Material()
    val reflectionPoint = point(0, 0, 0)

    val eyeVector = direction(0, -sqrt(2) / 2, -sqrt(2) / 2)
    val surfaceNormal = direction(0, 0, -1)

    val light = PointLight(point(0, 10, -10), color(1, 1, 1))

    val resultingColor = PhongLighting(material, light, reflectionPoint, eyeVector, surfaceNormal)

    // specular reflection should drop to almost 0
    resultingColor should approximatelyEqual (color(1.6364, 1.6364, 1.6364))
  }

  it should "work with might behind the surface" in {
    val material = Material()
    val reflectionPoint = point(0, 0, 0)

    val eyeVector = direction(0, 0, -1)
    val surfaceNormal = direction(0, 0, -1)

    val light = PointLight(point(0, 0, 10), color(1, 1, 1))

    val resultingColor = PhongLighting(material, light, reflectionPoint, eyeVector, surfaceNormal)

    // specular reflection should drop to almost 0
    resultingColor should approximatelyEqual (color(0.1, 0.1, 0.1))
  }

}
