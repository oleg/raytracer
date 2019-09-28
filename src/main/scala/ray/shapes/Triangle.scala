package ray.shapes

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Precision, Ray, Vector}

case class Triangle(p1: Point,
                    p2: Point,
                    p3: Point,
                    transform: Matrix4x4 = Matrix4x4.Identity,
                    material: Material = Material(),
                    parent: Shape = null) extends Shape {

  val e1: Vector = p2 - p1
  val e2: Vector = p3 - p1
  val normal: Vector = e2.cross(e1).normalize

  override def localIntersect(ray: Ray): Intersections = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (implicitly[Precision[Double]].approximatelyEqual(det, 0.0)) {
      return Intersections(Nil)
    }

    val f = 1.0 / det
    val p1ToOrigin = ray.origin - p1
    val u = f * p1ToOrigin.dot(dirCrossE2)
    if (u < 0 || 1 < u) {
      return Intersections(Nil)
    }

    val originCrossE1 = p1ToOrigin cross e1
    val v = f * ray.direction.dot(originCrossE1)
    if (v < 0 || 1 < (u + v)) {
      return Intersections(Nil)
    }

    val t = f * e2.dot(originCrossE1)
    Intersections(Intersection(t, this) :: Nil)
  }

  override def localNormalAt(point: Point, intersection: Intersection): Vector = normal

}

case class SmoothTriangle(
                           p1: Point,
                           p2: Point,
                           p3: Point,
                           n1: Vector,
                           n2: Vector,
                           n3: Vector,
                           transform: Matrix4x4 = Matrix4x4.Identity,
                           material: Material = Material(),
                           parent: Shape = null) extends Shape {

  val e1: Vector = p2 - p1
  val e2: Vector = p3 - p1
  val normal: Vector = e2.cross(e1).normalize

  override def localIntersect(ray: Ray): Intersections = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (implicitly[Precision[Double]].approximatelyEqual(det, 0.0)) {
      return Intersections(Nil)
    }

    val f = 1.0 / det
    val p1ToOrigin = ray.origin - p1
    val u = f * p1ToOrigin.dot(dirCrossE2)
    if (u < 0 || 1 < u) {
      return Intersections(Nil)
    }

    val originCrossE1 = p1ToOrigin cross e1
    val v = f * ray.direction.dot(originCrossE1)
    if (v < 0 || 1 < (u + v)) {
      return Intersections(Nil)
    }

    val t = f * e2.dot(originCrossE1)
    Intersections(Intersection(t, this, u, v) :: Nil)
  }

  override def localNormalAt(point: Point, hit: Intersection): Vector = {
    val vector0 = n2 * hit.u
    val vector1 = n3 * hit.v
    val vector2 = n1 * (1 - hit.u - hit.v)
    (vector0 + vector1 + vector2)
  }

}