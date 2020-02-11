package de.jjedele.raytracer.misc

import de.jjedele.raytracer.data.Vector4
import org.scalajs.dom.{CanvasRenderingContext2D, document}
import org.scalajs.dom.html.Canvas

/**
 * Example of a flying projectile.
 *
 * The putting it all together example for chapter 1 and 2.
 */
object ProjectileExample {

  case class Projectile(position: Vector4, velocity: Vector4)

  case class Environment(gravity: Vector4, wind: Vector4)

  def tick(projectile: Projectile, environment: Environment): Projectile =
    Projectile(
      (projectile.position + projectile.velocity).map(Math.max(_, 0)), // clip to 0
      projectile.velocity + environment.gravity + environment.wind)

  def main(args: Array[String]): Unit = {
    val environment = Environment(
      Vector4.vector(0, -0.1, 0),
      Vector4.vector(-0.01, 0, 0))

    var projectile = Projectile(
      Vector4.point(0, 2, 0),
      Vector4.vector(1, 1, 0).normalized)

    val canvas = document.createElement("canvas").asInstanceOf[Canvas]
    canvas.width = 500
    canvas.height = 500
    document.body.appendChild(canvas)

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    ctx.fillStyle = "rgb(0, 0, 0)"

    while (projectile.position.y > 0) {
      println(projectile.position)
      ctx.fillRect(projectile.position.x * 20, 100 - projectile.position.y * 20, 1, 1)
      projectile = tick(projectile, environment)
    }
  }

}
