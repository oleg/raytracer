package ray.shapes

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

case class Cube(transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material(),
                parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val (xtmin, xtmax) = normalizeAxis(ray.origin.x, ray.direction.x)
    val (ytmin, ytmax) = normalizeAxis(ray.origin.y, ray.direction.y)
    val (ztmin, ztmax) = normalizeAxis(ray.origin.z, ray.direction.z)

    val min = List(xtmin, ytmin, ztmin).max
    val max = List(xtmax, ytmax, ztmax).min

    if (min > max) Intersections(Nil)
    else Intersections(Intersection(min, this) :: Intersection(max, this) :: Nil)
  }

  private def normalizeAxis(origin: Double, direction: Double): (Double, Double) = {
    val (min, max) = ((-1 - origin) / direction, (1 - origin) / direction)
    if (min > max) (max, min) else (min, max)
  }

  override def localNormalAt(point: Point, intersection: Intersection): Vector = {
    val max = List(point.x, point.y, point.z).map(math.abs).max

    if (max == point.x.abs)
      return Vector(point.x, 0, 0)

    if (max == point.y.abs)
      return Vector(0, point.y, 0)

    Vector(0, 0, point.z)
  }

}
