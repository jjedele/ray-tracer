package de.jjedele.raytracer.misc

import de.jjedele.raytracer.data.Matrix
import org.scalajs.dom.{CanvasRenderingContext2D, document}
import org.scalajs.dom.html.Canvas

/**
 * Example of a flying projectile.
 *
 * The putting it all together example for chapter 1 and 2.
 */
object ProjectileExample {

  import Matrix._

  case class Projectile(position: Vector, velocity: Vector)

  case class Environment(gravity: Vector, wind: Vector)

  def tick(projectile: Projectile, environment: Environment): Projectile =
    Projectile(
      (projectile.position + projectile.velocity).map(Math.max(_, 0)), // clip to 0
      projectile.velocity + environment.gravity + environment.wind)

  def main(args: Array[String]): Unit = {
    val environment = Environment(
      direction(0, -0.1, 0),
      direction(-0.01, 0, 0))

    var projectile = Projectile(
      point(0, 2, 0),
      direction(1, 1, 0).normalized)

    val canvas = document.createElement("canvas").asInstanceOf[Canvas]
    canvas.width = 500
    canvas.height = 500
    document.body.appendChild(canvas)

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    ctx.fillStyle = "rgb(0, 0, 0)"

    while (projectile.position(1) > 0) {
      println(projectile.position)
      ctx.fillRect(projectile.position(0) * 20, 100 - projectile.position(1) * 20, 1, 1)
      projectile = tick(projectile, environment)
    }
  }

}
