package de.jjedele.raytracer.lighting

import de.jjedele.raytracer.Matrix

/**
 * Material descriptor for objects.
 *
 * @param color
 * @param ambient Ambient light is reflected from other objects and treated as constant by the Phong model.
 *                Should be between 0 and 1.
 * @param diffuse Light reflected from a matte surface.
 *                Should be between 0 and 1.
 * @param specular Reflection of the light source itself.
 *                 Should be between 0 and 1.
 * @param shininess Values between 10 and 200 typically work well.
 */
case class Material (color: Matrix = Matrix.color(1, 1, 1),
                     ambient: Double = 0.1,
                     diffuse: Double = 0.9,
                     specular: Double = 0.9,
                     shininess: Double = 200.0)
