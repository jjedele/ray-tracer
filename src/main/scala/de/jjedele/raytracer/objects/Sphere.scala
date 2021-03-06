package de.jjedele.raytracer.objects

import de.jjedele.raytracer.lighting.Material
import de.jjedele.raytracer.{Matrix, Transform}
import de.jjedele.raytracer.ray.{Intersection, Intersections, Ray}

/**
 * A sphere.
 */
case class Sphere(transform: Transform = Transform(), material: Material = Material()) extends GeometricObject {

  val center = Matrix.point(0, 0, 0)
  val radius = 1.0

  /**
   * Intersect the object with given ray.
   *
   * @param ray
   * @return The intersection points.
   */
  override def intersect(ray: Ray): Intersections = {
    val localRay = ray.transformBy(this.transform.inverse.get)
    val sphereToRay = localRay.origin - this.center

    val a = localRay.direction inner localRay.direction
    val b = localRay.direction inner sphereToRay * 2
    val c = (sphereToRay inner sphereToRay) - 1

    val discriminant = math.pow(b, 2) - 4 * a * c

    if (discriminant < 0)
      Intersections()
    else
      Intersections(
        Intersection((-b - math.sqrt(discriminant)) / (2 * a), ray, this),
        Intersection((-b + math.sqrt(discriminant)) / (2 * a), ray, this))
  }

  /**
   * Change the transformation associated with this object.
   * @param transform
   * @return
   */
  override def withTransform(transform: Transform): Sphere =
    this.copy(transform=transform)

  /**
   * Calculate the surface normal at given point.
   *
   * @param point Point is assumed to be on the object. No further checks are done.
   * @return
   */
  override def normalAt(point: Matrix): Matrix = {
    // TODO handle non-invertible transformations better
    val pointInObjectSpace = transform.inverse().get.apply(point)

    val normalInObjectSpace = pointInObjectSpace - center

    val normalInWorldSpace = transform.inverse().get.combined.transposed matmul normalInObjectSpace
    // this is a hack because we actually would need to take the submatrix of the transform at 3,3 not to mess with w
    normalInWorldSpace(3, 0) = 0

    normalInWorldSpace.normalized
  }

}
