package ray.shapes

import ray.raymath.RayMath
import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material(),
                  var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val sphereToRay = ray.origin - Point(0, 0, 0)
    val a: Double = ray.direction dot ray.direction
    val b: Double = 2 * (ray.direction dot sphereToRay)
    val c: Double = (sphereToRay dot sphereToRay) - 1

    RayMath.solveQuadratic(a, b, c)
      .map(t => Intersections(Intersection(t._1, this) :: Intersection(t._2, this) :: Nil))
      .getOrElse(Intersections.EMPTY)
  }

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    point - Point(0, 0, 0)

}

object Sphere {

  def glass(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Sphere =
    Sphere(transform, material)

}

