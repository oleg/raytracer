package ray.tracer.shapemath

import ray.tracer
import ray.tracer.{Point, Precision, Ray, Vector, PrecisionDouble}

case class SmoothTriangleMath(p1: Point,
                              p2: Point,
                              p3: Point,
                              n1: Vector,
                              n2: Vector,
                              n3: Vector) extends ShapeMath {
  val e1: Vector = p2 - p1
  val e2: Vector = p3 - p1
  val normal: Vector = e2.cross(e1).normalize


  override def intersect(ray: Ray): List[Inter] = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (summon[Precision[Double]].approximatelyEqual(det, 0.0)) {
      return Nil
    }

    val f = 1.0 / det
    val p1ToOrigin = ray.origin - p1
    val u = f * p1ToOrigin.dot(dirCrossE2)
    if (u < 0 || 1 < u) {
      return Nil
    }

    val originCrossE1 = p1ToOrigin cross e1
    val v = f * ray.direction.dot(originCrossE1)
    if (v < 0 || 1 < (u + v)) {
      return Nil
    }

    val t = f * e2.dot(originCrossE1)
    Inter(t, u, v) :: Nil
  }


  override def normalAt(point: Point, inter: Inter): tracer.Vector = {
    val vector0 = n2 * inter.u
    val vector1 = n3 * inter.v
    val vector2 = n1 * (1 - inter.u - inter.v)
    vector0 + vector1 + vector2
  }
}
