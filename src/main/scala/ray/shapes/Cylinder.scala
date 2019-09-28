package ray.shapes

import ray.raymath.RayMath
import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Precision, Ray, Vector}

case class Cylinder(minimum: Double = Double.NegativeInfinity,
                    maximum: Double = Double.PositiveInfinity,
                    closed: Boolean = false,
                    transform: Matrix4x4 = Matrix4x4.Identity,
                    material: Material = Material(),
                    var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val d = ray.direction
    val o = ray.origin

    val a = d.x * d.x + d.z * d.z
    val b = 2 * o.x * d.x + 2 * o.z * d.z
    val c = o.x * o.x + o.z * o.z - 1

    val xsCaps = intersectCaps(ray)
    val p = implicitly[Precision[Double]]
    if (p.approximatelyEqual(a, 0.0)) {
      return Intersections(xsCaps) //todo refactor
    }

    RayMath.solveQuadratic(a, b, c) match {
      case Some((t0, t1)) =>
        val xsCylinder = List(t0, t1)
          .sorted
          .map(t => (t, o.y + t * d.y))
          .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
          .map(t2y => t2y._1)
          .map(Intersection(_, this))
        Intersections(xsCaps ::: xsCylinder)
      case None => Intersections(xsCaps)
    }
  }

  private def intersectCaps(ray: Ray): List[Intersection] = {
    var result: List[Intersection] = Nil //todo refactor

    if (!closed || implicitly[Precision[Double]].approximatelyEqual(ray.direction.y, 0.0)) {
      return result
    }

    val t0 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t0)) {
      result ::= Intersection(t0, this)
    }

    val t1 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1)) {
      result ::= Intersection(t1, this)
    }

    result
  }

  private def checkCap(ray: Ray, t: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    (x * x + z * z) <= 1
  }

  override def localNormalAt(point: Point, intersection: Intersection): Vector = {
    val dist = point.x * point.x + point.z * point.z
    val p = implicitly[Precision[Double]] //todo fix me

    if (dist < 1 && p.approximatelyGreater(point.y, maximum))
      return Vector(0, 1, 0)

    if (dist < 1 && p.approximatelyLess(point.y, minimum))
      return Vector(0, -1, 0)

    Vector(point.x, 0, point.z)
  }
}

//todo refactor this class it's similar to Cylinder
case class Cone(minimum: Double = Double.NegativeInfinity,
                maximum: Double = Double.PositiveInfinity,
                closed: Boolean = false,
                transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material(),
                var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val d = ray.direction
    val o = ray.origin

    val a = d.x * d.x - d.y * d.y + d.z * d.z
    val b = 2 * o.x * d.x - 2 * o.y * d.y + 2 * o.z * d.z
    val c = o.x * o.x - o.y * o.y + o.z * o.z

    val xsCaps = intersectCaps(ray)
    val p = implicitly[Precision[Double]]
    if (p.approximatelyEqual(a, 0.0) && !p.approximatelyEqual(b, 0.0)) {
      val t = -c / (2 * b)
      val y0 = o.y + (t * d.y)
      if (minimum < y0 && y0 < maximum) {
        return Intersections(xsCaps ::: Intersection(t, this) :: Nil) //todo refactor
      }
    }

    RayMath.solveQuadratic(a, b, c) match {
      case Some((t0, t1)) =>
        val xsCylinder = List(t0, t1)
          .sorted
          .map(t => (t, o.y + t * d.y))
          .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
          .map(t2y => t2y._1)
          .map(Intersection(_, this))
        Intersections(xsCaps ::: xsCylinder)
      case None => Intersections(xsCaps)
    }
  }

  private def intersectCaps(ray: Ray): List[Intersection] = {
    var result: List[Intersection] = Nil //todo refactor

    if (!closed || implicitly[Precision[Double]].approximatelyEqual(ray.direction.y, 0.0)) {
      return result
    }

    val t0 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t0, minimum)) {
      result ::= Intersection(t0, this)
    }

    val t1 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1, maximum)) {
      result ::= Intersection(t1, this)
    }

    result
  }

  private def checkCap(ray: Ray, t: Double, radius: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    (x * x + z * z) <= radius * radius
  }

  override def localNormalAt(point: Point, intersection: Intersection): Vector = {
    val dist = point.x * point.x + point.z * point.z
    val p = implicitly[Precision[Double]]

    if (dist < 1 && p.approximatelyGreater(point.y, maximum)) //todo remove reference to epsilon
      return Vector(0, 1, 0)

    if (dist < 1 && p.approximatelyLess(point.y, minimum))
      return Vector(0, -1, 0)

    val y0 = math.hypot(point.x, point.z)
    val y = if (point.y > 0) -y0 else y0
    Vector(point.x, y, point.z)
  }
}