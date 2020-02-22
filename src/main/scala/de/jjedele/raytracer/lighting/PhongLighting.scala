package de.jjedele.raytracer.lighting

import de.jjedele.raytracer.Matrix

/**
 * Implementation of lighting based on the Phong reflection model.
 */
object PhongLighting {

  import Matrix._

  val Black = color(0, 0, 0)

  /**
   * Apply lighting.
   * @param material Material of the reflecting object.
   * @param light Light source.
   * @param reflectionPoint Position of the reflection.
   * @param eyeVector Eye vector.
   * @param normal Normal of the reflecting surface.
   * @return
   */
  def apply(material: Material, light: PointLight, reflectionPoint: Matrix, eyeVector: Matrix, normal: Matrix): Matrix = {
    // combine material and light color
    val effectiveColor = material.color hadamard light.intensity

    // calculate direction of light
    val lightVector = (light.position - reflectionPoint).normalized
    val lightNormalAngle = lightVector inner normal

    // reflection
    val reflectionVector = (-lightVector).reflectAt(normal)
    val reflectionEyeAngle = reflectionVector inner eyeVector

    // individual contributions
    val ambientContribution = effectiveColor * material.ambient

    val diffuseContribution =
      if (lightNormalAngle <= 0)
        // light is on the other side of the surface
        Black
      else
        effectiveColor * material.diffuse * lightNormalAngle

    val specularContribution =
      if (lightNormalAngle > 0 && reflectionEyeAngle > 0) {
        val factor = math.pow(reflectionEyeAngle, material.shininess)
        light.intensity * material.specular * factor
      } else {
        Black
      }

    ambientContribution + diffuseContribution + specularContribution
  }

}
