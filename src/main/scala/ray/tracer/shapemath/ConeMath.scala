package ray.tracer.shapemath

import ray.tracer.{Point, Precision, Ray, Vector}

case class ConeMath(minimum: Double = Double.NegativeInfinity,
                    maximum: Double = Double.PositiveInfinity,
                    closed: Boolean = false) extends ShapeMath {


  override def intersect(ray: Ray): List[Inter] = {
    val a = ray.direction.x * ray.direction.x - ray.direction.y * ray.direction.y + ray.direction.z * ray.direction.z
    val b = 2 * ray.origin.x * ray.direction.x - 2 * ray.origin.y * ray.direction.y + 2 * ray.origin.z * ray.direction.z
    val c = ray.origin.x * ray.origin.x - ray.origin.y * ray.origin.y + ray.origin.z * ray.origin.z

    val p = implicitly[Precision[Double]]
    if (p.approximatelyEqual(a, 0.0) && !p.approximatelyEqual(b, 0.0)) {
      val t = -c / (2 * b)
      val y0 = ray.origin.y + (t * ray.direction.y)
      if (minimum < y0 && y0 < maximum) {
        return intersectCaps(ray) ::: Inter(t) :: Nil //todo refactor
      }
    }

    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      return intersectCaps(ray)
    }

    //TODO implement new math object for solving this
    val t0 = (-b - math.sqrt(discriminant)) / (2 * a)
    val t1 = (-b + math.sqrt(discriminant)) / (2 * a)

    val xsCylinder = List(t0, t1)
      .sorted
      .map(t => (t, ray.origin.y + t * ray.direction.y))
      .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
      .map(t2y => Inter(t2y._1))

    val xsCaps = intersectCaps(ray)

    xsCaps ::: xsCylinder
  }

  override def normalAt(point: Point, inter: Inter): Vector = {
    val dist = point.x * point.x + point.z * point.z
    //p.approximatelyLess(point.y, minimum)
    val p = implicitly[Precision[Double]]
    if (dist < 1 && p.approximatelyGreater(point.y, maximum)) { //todo remove reference to epsilon
      Vector(0, 1, 0)
    } else if (dist < 1 && p.approximatelyLess(point.y, minimum)) {
      Vector(0, -1, 0)
    } else {
      val y0 = math.hypot(point.x, point.z)
      val y = if (point.y > 0) -y0 else y0
      Vector(point.x, y, point.z)
    }
  }

  private def intersectCaps(ray: Ray): List[Inter] = {
    var result: List[Inter] = Nil //todo refactor

    val p = implicitly[Precision[Double]]
    if (!closed || p.approximatelyEqual(ray.direction.y, 0.0)) {
      return result
    }

    val t0 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t0, minimum)) {
      result ::= Inter(t0)
    }

    val t1 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1, maximum)) {
      result ::= Inter(t1)
    }

    result
  }

  private def checkCap(ray: Ray, t: Double, radius: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    (x * x + z * z) <= radius * radius
  }


}
