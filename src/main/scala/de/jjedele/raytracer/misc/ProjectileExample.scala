package de.jjedele.raytracer.misc

import java.io.PrintWriter

import de.jjedele.raytracer.{Canvas, JSConverters, Matrix}
import org.scalajs.dom

/**
 * Example of a flying projectile.
 *
 * The putting it all together example for chapter 1 and 2.
 */
object ProjectileExample {

  import Matrix._

  case class Projectile(position: Vector, velocity: Vector)

  case class Environment(gravity: Vector, wind: Vector)

  // forward projectile by one time step
  def tick(projectile: Projectile, environment: Environment): Projectile =
    Projectile(
      (projectile.position + projectile.velocity).map(Math.max(_, 0)), // clip to 0
      projectile.velocity + environment.gravity + environment.wind)

  def main(args: Array[String]): Unit = {
    val environment = Environment(
      gravity = direction(0, -0.1, 0),
      wind = direction(-0.01, 0, 0))

    var projectile = Projectile(
      position = point(0, 1, 0),
      velocity = direction(1, 1.8, 0).normalized * 11.25)

    val canvas = new Canvas(height = 500, width = 900)

    val red = color(1.0, 0, 0)
    while (projectile.position(1) > 0) {
      canvas.paint(
        projectile.position(0).toInt,
        projectile.position(1).toInt,
        red
      )

      projectile = tick(projectile, environment)
    }

//    val writer = new PrintWriter("projectile.ppm")
//    writer.write(canvas.ppm)
//    writer.close()

    dom.document.body.appendChild(JSConverters.toHTMLCanvas(canvas))
  }

}
