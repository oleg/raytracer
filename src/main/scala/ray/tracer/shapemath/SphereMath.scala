package ray.tracer.shapemath

import ray.tracer.{Point, Ray, Vector}

case class SphereMath() extends ShapeMath {
  override def intersect(ray: Ray): List[Inter] = {
    val sphereToRay = ray.origin - Point(0, 0, 0)

    val a: Double = ray.direction dot ray.direction
    val b: Double = 2 * (ray.direction dot sphereToRay)
    val c: Double = (sphereToRay dot sphereToRay) - 1

    val discriminant = b * b - 4 * a * c

    if (discriminant >= 0) {
      val sd = math.sqrt(discriminant)
      val t1 = (-b - sd) / (2 * a)
      val t2 = (-b + sd) / (2 * a)
      Inter(t1) :: Inter(t2) :: Nil
    } else {
      Nil
    }
  }

  override def normalAt(point: Point, inter: Inter): Vector =
    point - Point(0, 0, 0)
}