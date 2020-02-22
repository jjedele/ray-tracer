package de.jjedele.raytracer.lighting

import de.jjedele.raytracer.Matrix

/**
 * A point light source without size.
 *
 * @param position
 * @param intensity
 */
case class PointLight(position: Matrix, intensity: Matrix)
