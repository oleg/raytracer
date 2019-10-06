package ray.tracer.shapemath

import ray.tracer.{Point, Precision, Ray, Vector}

case class CylinderMath(minimum: Double = Double.NegativeInfinity,
                        maximum: Double = Double.PositiveInfinity,
                        closed: Boolean = false) extends ShapeMath {


  override def intersect(ray: Ray): List[Inter] = {
    val a = ray.direction.x * ray.direction.x + ray.direction.z * ray.direction.z
    val b = 2 * ray.origin.x * ray.direction.x + 2 * ray.origin.z * ray.direction.z
    val c = ray.origin.x * ray.origin.x + ray.origin.z * ray.origin.z - 1

    val xsCaps = intersectCaps(ray)
    if (implicitly[Precision[Double]].approximatelyEqual(a, 0.0)) {
      return xsCaps //todo refactor
    }

    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      return xsCaps
    }

    //TODO implement new math object for solving this
    val t0 = (-b - math.sqrt(discriminant)) / (2 * a)
    val t1 = (-b + math.sqrt(discriminant)) / (2 * a)

    val xsCylinder = List(t0, t1)
      .sorted
      .map(t => (t, ray.origin.y + t * ray.direction.y))
      .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
      .map(t2y => Inter(t2y._1))

    xsCaps ::: xsCylinder
  }

  override def normalAt(point: Point, inter: Inter): Vector = {
    val dist = point.x * point.x + point.z * point.z
    val p = implicitly[Precision[Double]] //todo fix me
    if (dist < 1 && p.approximatelyGreater(point.y, maximum)) {
      Vector(0, 1, 0)
    } else if (dist < 1 && p.approximatelyLess(point.y, minimum)) {
      Vector(0, -1, 0)
    } else {
      Vector(point.x, 0, point.z)
    }
  }

  private def intersectCaps(ray: Ray): List[Inter] = {
    var result: List[Inter] = Nil //todo refactor

    if (!closed || implicitly[Precision[Double]].approximatelyEqual(ray.direction.y, 0.0)) {
      return result
    }

    val t0 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t0)) {
      result ::= Inter(t0)
    }

    val t1 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1)) {
      result ::= Inter(t1)
    }

    result
  }

  private def checkCap(ray: Ray, t: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    (x * x + z * z) <= 1
  }

}
