package ray.tracer.shapemath

import ray.tracer.{Point, Ray, Vector}

case class CubeMath() extends ShapeMath {

  override def intersect(ray: Ray): List[Inter] = {
    val (xtmin, xtmax) = checkAxis(ray.origin.x, ray.direction.x)
    val (ytmin, ytmax) = checkAxis(ray.origin.y, ray.direction.y)
    val (ztmin, ztmax) = checkAxis(ray.origin.z, ray.direction.z)

    val tmin = List(xtmin, ytmin, ztmin).max
    val tmax = List(xtmax, ytmax, ztmax).min

    if (tmin > tmax) Nil
    else Inter(tmin) :: Inter(tmax) :: Nil
  }

  private def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val (tmin, tmax) = ((-1 - origin) / direction, (1 - origin) / direction)
    if (tmin > tmax) (tmax, tmin) else (tmin, tmax)
  }

  override def normalAt(point: Point, inter: Inter): Vector = {
    val maxc = List(point.x, point.y, point.z).map(math.abs).max

    if (maxc == point.x.abs)
      Vector(point.x, 0, 0)
    else if (maxc == point.y.abs)
      Vector(0, point.y, 0)
    else
      Vector(0, 0, point.z)
  }

}