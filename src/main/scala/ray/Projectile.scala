package ray

import java.io.PrintWriter

object Main {

  type Vector = Tuple
  type Point = Tuple

  class Environment(val gravity: Vector, val wind: Vector)

  class Projectile(val position: Point, val velocity: Vector) {

    def tick(env: Environment): Projectile =
      new Projectile(
        position + velocity,
        velocity + env.gravity + env.wind)

  }

  def main(args: Array[String]): Unit = {
    draw(args)
  }

  def draw(args: Array[String]): Unit = {
    val canvas = Canvas(900, 500)
    val red = Color(1, 0, 0)
    val environment = new Environment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))
    var projectile = new Projectile(Point(0, 1, 0), Vector(1, 1.8, 0).normalize * 11.25)
    do {
      canvas.pixels(projectile.position.x.toInt)(canvas.height - projectile.position.y.toInt - 1) = red
      projectile = projectile.tick(environment)
    } while (projectile.position.y > 0)

    new PrintWriter("picture.ppm") {
      write(canvas.toPpm())
      close()
    }
  }

  def run(args: Array[String]): Unit = {
    val environment = new Environment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))
    var projectile = new Projectile(Point(0, 1, 0), Vector(1, 1, 0).normalize)
    do {
      projectile = projectile.tick(environment)

      println(projectile.position)
      Thread.sleep(500)
    } while (projectile.position.y > 0)
  }

}



