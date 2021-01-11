package it.ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.{Canvas, Color, Point, Vector}
import testutil.Sources

class ProjectileIntegrationTest extends AnyFunSuite {

  class Environment(val gravity: Vector, val wind: Vector)

  class Projectile(val position: Point, val velocity: Vector) {

    def tick(env: Environment): Projectile =
      new Projectile(
        position + velocity,
        velocity + env.gravity + env.wind)
  }

  test("generate projectile") {
    val canvas = Canvas(900, 500)
    val red = Color(1, 1, 1)
    val environment = new Environment(Vector(0, -0.15, 0), Vector(-0.02, 0, 0))
    var projectile = new Projectile(Point(0, 1, 0), Vector(1, 1.8, 0).normalize * 11.25)

    while ({{
      canvas(projectile.position.x.toInt, canvas.height - projectile.position.y.toInt - 1) = red
      projectile = projectile.tick(environment)

    }; projectile.position.y > 0 && projectile.position.y < canvas.height }) ()

//    canvas.dumpPpmTo("projectile-900x500.ppm")
    assert(canvas.toPpm == Sources.readString("/it/projectile-900x500.ppm"))
  }

}
