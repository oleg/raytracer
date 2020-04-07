package it.ray.tracer
import ray.tracer.shape.ShapeFactory._
import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.Matrix4x4.Identity
import ray.tracer._
import testutil.Sources
import ray.tracer.shape.ShapeFactory._
import scala.math.Pi

class ReflectionIntegrationTest extends AnyFunSuite {

  test("generate reflections") {

    val floor = Plane(
      material = Material(
        reflective = 0.5
//        ,pattern = RingPattern(Color.black, Color.white)
      ))

    val back = Plane(
      material = Material(
        reflective = 0.1,
        pattern = CheckersPattern(Color.black, Color.white)),
      transform = Identity.rotateX(-Pi / 2).translate(0, 0, 4)
    )

    val left = Sphere(
      Identity.scale(1, 0.33, 0.33).translate(-1.5, 0.33, -0.75),
      Material(
        pattern = GradientPattern(
          Color(0.1, 0.8, 0.3),
          Color(0.8, 0.3, 0.6),
          transform = Identity.scale(0.3, 0.3, 0.3)),
        color = Color(1, 0.1, 0.1),
        diffuse = 0.7,
        specular = 0.3))

    val middle = Sphere(
      Identity.translate(-0.5, 1, 0.2),
      Material(
        color = Color(0.1, 0.1, 1),
        diffuse = 0.7,
        specular = 0.3,
        pattern = ColorPattern(
          transform = Matrix4x4.Scaling(0.5, 0.5, 0.5))))

    val right = Sphere(
      Identity.scale(0.5, 0.8, 0.5).translate(1.5, 0.5, -0.5),
      Material(
        pattern = GradientPattern(
          Color(1, 0.3, 0.5),
          Color(0.5, 0.3, 1),
          transform = Identity.rotateX(Pi / 2).rotateY(Pi / 2).rotateZ(Pi / 2)),
        color = Color(0.1, 1, 0.1),
        diffuse = 0.7,
        specular = 0.3))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, back :: floor :: middle :: right :: left :: Nil)
    val camera = Camera(500, 250, Pi / 3, Matrix4x4.viewTransform(Point(0, 3, -6), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //    canvas.dumpPpmTo("reflections-500x250.ppm")
    assert(canvas.toPpm == Sources.readString("/it/reflections-500x250.ppm"))
  }

}
