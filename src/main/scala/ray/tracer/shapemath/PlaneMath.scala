package ray.tracer.shapemath

import ray.tracer.{Point, Precision, Ray, Vector}

case class PlaneMath() extends ShapeMath {

  override def intersect(ray: Ray): List[Inter] = {
    val p = implicitly[Precision[Double]]
    if (!p.approximatelyEqual(ray.direction.y, 0.0)) {
      val t = -ray.origin.y / ray.direction.y
      Inter(t) :: Nil
    } else {
      Nil
    }
  }

  override def normalAt(point: Point, inter: Inter): Vector =
    Vector(0, 1, 0)
}